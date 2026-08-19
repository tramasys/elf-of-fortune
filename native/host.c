#define _GNU_SOURCE

#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <limits.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/random.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define RESPONSE_NAME ".elf-of-fortune.response"
#define ERROR_MARKER "@@ELF_OF_FORTUNE_ERROR@@"
#define SLEEP_MARKER "@@ELF_OF_FORTUNE_SLEEP@@"

struct input_file {
    unsigned char *bytes;
    size_t size;
    mode_t mode;
    bool ok;
    char error[256];
};

static void host_error(const char *message) {
    fprintf(stderr, "error: %s\n", message);
}

static void host_errno(const char *action, const char *path) {
    fprintf(stderr, "error: %s '%s': %s\n", action, path, strerror(errno));
}

static bool contains_newline(const char *text) {
    return strchr(text, '\n') != NULL || strchr(text, '\r') != NULL;
}

static int write_all(int fd, const unsigned char *data, size_t size) {
    while (size > 0) {
        ssize_t written = write(fd, data, size);

        if (written < 0) {
            if (errno == EINTR) continue;

            return -1;
        }

        data += (size_t)written;
        size -= (size_t)written;
    }

    return 0;
}

static struct input_file read_input(const char *path) {
    struct input_file result = {0};

    result.mode = 0755;

    if (path == NULL) {
        snprintf(result.error, sizeof(result.error), "no input candidate");
        return result;
    }

    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) {
        snprintf(result.error, sizeof(result.error), "%s", strerror(errno));
        return result;
    }

    struct stat info;
    if (fstat(fd, &info) < 0) {
        snprintf(result.error, sizeof(result.error), "%s", strerror(errno));
        close(fd);
        return result;
    }

    if (!S_ISREG(info.st_mode)) {
        snprintf(result.error, sizeof(result.error), "not a regular file");
        close(fd);
        return result;
    }

    if (info.st_size < 0 || (uintmax_t)info.st_size > (uintmax_t)INT32_MAX / 2u) {
        snprintf(result.error, sizeof(result.error), "file is too large for Kitten's List index representation");
        close(fd);
        return result;
    }

    result.size = (size_t)info.st_size;
    result.mode = info.st_mode & 07777;
    result.bytes = malloc(result.size == 0 ? 1 : result.size);
    if (result.bytes == NULL) {
        snprintf(result.error, sizeof(result.error), "out of memory");
        close(fd);
        return result;
    }

    size_t used = 0;

    while (used < result.size) {
        ssize_t count = read(fd, result.bytes + used, result.size - used);

        if (count < 0) {
            if (errno == EINTR) continue;

            snprintf(result.error, sizeof(result.error), "%s", strerror(errno));
            free(result.bytes);
            result.bytes = NULL;
            close(fd);
            return result;
        }

        if (count == 0) {
            snprintf(result.error, sizeof(result.error), "file changed while reading");
            free(result.bytes);
            result.bytes = NULL;
            close(fd);
            return result;
        }

        used += (size_t)count;
    }

    close(fd);
    result.ok = true;

    return result;
}

static uint64_t entropy_seed(void) {
    uint64_t seed = 0;
    ssize_t count;

    do {
        count = getrandom(&seed, sizeof(seed), 0);
    } while (count < 0 && errno == EINTR);

    if (count == (ssize_t)sizeof(seed)) return seed;

    int fd = open("/dev/urandom", O_RDONLY | O_CLOEXEC);

    if (fd >= 0) {
        size_t used = 0;

        while (used < sizeof(seed)) {
            ssize_t got = read(fd, (unsigned char *)&seed + used, sizeof(seed) - used);

            if (got < 0 && errno == EINTR) continue;
            if (got <= 0) break;

            used += (size_t)got;
        }

        close(fd);

        if (used == sizeof(seed)) return seed;
    }

    return (uint64_t)time(NULL) ^ ((uint64_t)getpid() << 32);
}

static int make_paths(char **kitten_path, char **source_dir) {
    const char *kitten_env = getenv("ELF_OF_FORTUNE_KITTEN");
    const char *source_env = getenv("ELF_OF_FORTUNE_SOURCE_DIR");

    if (kitten_env != NULL && source_env != NULL) {
        *kitten_path = strdup(kitten_env);
        *source_dir = strdup(source_env);

        return (*kitten_path != NULL && *source_dir != NULL) ? 0 : -1;
    }

    char executable[PATH_MAX];
    ssize_t length = readlink("/proc/self/exe", executable, sizeof(executable) - 1);

    if (length < 0 || length >= (ssize_t)sizeof(executable) - 1) return -1;

    executable[length] = '\0';

    char *slash = strrchr(executable, '/');

    if (slash == NULL) return -1;

    *slash = '\0';

    if (asprintf(kitten_path, "%s/../libexec/elf-of-fortune/kitten", executable) < 0) return -1;

    if (asprintf(source_dir, "%s/../share/elf-of-fortune", executable) < 0) {
        free(*kitten_path);
        *kitten_path = NULL;

        return -1;
    }

    return 0;
}

static char *joined(const char *directory, const char *name) {
    char *result = NULL;

    if (asprintf(&result, "%s/%s", directory, name) < 0) return NULL;

    return result;
}

static void send_hex(FILE *stream, const unsigned char *bytes, size_t size) {
    static const char digits[] = "0123456789abcdef";
    for (size_t i = 0; i < size; ++i) {
        fputc(digits[bytes[i] >> 4], stream);
        fputc(digits[bytes[i] & 15], stream);
    }
}

static int relay_output(FILE *stream) {
    char *line = NULL;
    size_t capacity = 0;
    ssize_t length;

    while ((length = getline(&line, &capacity, stream)) >= 0) {
        if (strncmp(line, SLEEP_MARKER, strlen(SLEEP_MARKER)) == 0) {
            char *end = NULL;
            long milliseconds = strtol(line + strlen(SLEEP_MARKER), &end, 10);

            if (end != line + strlen(SLEEP_MARKER) && milliseconds >= 0 && milliseconds <= 10000) {
                struct timespec delay = {
                    .tv_sec = milliseconds / 1000,
                    .tv_nsec = (milliseconds % 1000) * 1000000L,
                };

                while (nanosleep(&delay, &delay) < 0 && errno == EINTR) {}
            }

            continue;
        }

        if (strncmp(line, ERROR_MARKER, strlen(ERROR_MARKER)) == 0) {
            fwrite(line + strlen(ERROR_MARKER), 1, (size_t)length - strlen(ERROR_MARKER), stderr);
            fflush(stderr);
            continue;
        }

        fwrite(line, 1, (size_t)length, stdout);
        fflush(stdout);
    }

    free(line);

    return ferror(stream) ? -1 : 0;
}

static void trim_line(char *line) {
    size_t length = strlen(line);

    while (length > 0 && (line[length - 1] == '\n' || line[length - 1] == '\r')) {
        line[--length] = '\0';
    }
}

static int hex_value(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

static unsigned char *decode_hex(const char *hex, size_t *output_size) {
    size_t length = strlen(hex);

    if ((length & 1u) != 0) return NULL;

    unsigned char *result = malloc(length == 0 ? 1 : length / 2);

    if (result == NULL) return NULL;

    for (size_t i = 0; i < length; i += 2) {
        int high = hex_value(hex[i]);
        int low = hex_value(hex[i + 1]);

        if (high < 0 || low < 0) {
            free(result);
            return NULL;
        }

        result[i / 2] = (unsigned char)((high << 4) | low);
    }

    *output_size = length / 2;

    return result;
}

static bool parse_size(const char *text, size_t *value) {
    if (*text == '\0' || *text == '-') return false;

    errno = 0;

    char *end = NULL;
    uintmax_t parsed = strtoumax(text, &end, 10);

    if (errno != 0 || *end != '\0' || parsed > SIZE_MAX) return false;

    *value = (size_t)parsed;

    return true;
}

static int atomic_write_file(const char *path, const unsigned char *data, size_t size, mode_t mode) {
    char *temporary = NULL;

    if (asprintf(&temporary, "%s.tmp.XXXXXX", path) < 0) return -1;

    int fd = mkstemp(temporary);

    if (fd < 0) {
        free(temporary);
        return -1;
    }

    int result = 0;

    if (fchmod(fd, mode) < 0 || write_all(fd, data, size) < 0 || fsync(fd) < 0 || close(fd) < 0) {
        result = -1;
    } else if (rename(temporary, path) < 0) {
        result = -1;
    }

    if (result < 0) {
        int saved = errno;

        close(fd);
        unlink(temporary);
        errno = saved;
    }

    free(temporary);

    return result;
}

static bool same_existing_file(const char *left, const char *right) {
    struct stat a, b;

    return stat(left, &a) == 0 && stat(right, &b) == 0 && a.st_dev == b.st_dev && a.st_ino == b.st_ino;
}

static void cleanup_runtime(const char *directory) {
    char *response = joined(directory, RESPONSE_NAME);

    if (response != NULL) {
        unlink(response);
        free(response);
    }

    rmdir(directory);
}

int main(int argc, char **argv) {
    for (int i = 1; i < argc; ++i) {
        if (contains_newline(argv[i])) {
            host_error("newlines in command-line arguments are unsupported");
            return 2;
        }
    }

    const char *candidate = argc > 1 ? argv[argc - 1] : NULL;
    struct input_file input = read_input(candidate);

    char *kitten = NULL;
    char *sources = NULL;

    if (make_paths(&kitten, &sources) < 0) {
        host_error("cannot locate Kitten runtime files");
        free(input.bytes);
        return 1;
    }

    char runtime_template[] = "/tmp/elf-of-fortune.XXXXXX";
    char *runtime = mkdtemp(runtime_template);

    if (runtime == NULL) {
        host_errno("cannot create runtime directory", runtime_template);
        free(kitten);
        free(sources);
        free(input.bytes);
        return 1;
    }

    int child_input[2];
    int child_output[2];

    if (pipe2(child_input, O_CLOEXEC) < 0 || pipe2(child_output, O_CLOEXEC) < 0) {
        host_error("cannot create Kitten transport pipes");
        cleanup_runtime(runtime);
        free(kitten);
        free(sources);
        free(input.bytes);
        return 1;
    }

    char *source_paths[6] = {
        joined(sources, "bytes.ktn"),
        joined(sources, "rng.ktn"),
        joined(sources, "roulette.ktn"),
        joined(sources, "elf.ktn"),
        joined(sources, "terminal.ktn"),
        joined(sources, "main.ktn"),
    };

    for (size_t i = 0; i < 6; ++i) {
        if (source_paths[i] == NULL) {
            host_error("out of memory while locating Kitten sources");
            cleanup_runtime(runtime);
            return 1;
        }
    }

    pid_t child = fork();

    if (child < 0) {
        host_error("cannot start Kitten interpreter");
        cleanup_runtime(runtime);
        return 1;
    }

    if (child == 0) {
        close(child_input[1]);
        close(child_output[0]);

        if (dup2(child_input[0], STDIN_FILENO) < 0 || dup2(child_output[1], STDOUT_FILENO) < 0 || chdir(runtime) < 0) {
            perror("elf-of-fortune host");
            _exit(127);
        }

        close(child_input[0]);
        close(child_output[1]);

        const char *kitten_data = getenv("ELF_OF_FORTUNE_KITTEN_DATADIR");

        setenv("Kitten_datadir", kitten_data == NULL ? sources : kitten_data, 1);

        char *const child_argv[] = {
            kitten,
            source_paths[0],
            source_paths[1],
            source_paths[2],
            source_paths[3],
            source_paths[4],
            source_paths[5],
            NULL,
        };

        execv(kitten, child_argv);
        perror("cannot execute Kitten");
        _exit(127);
    }

    close(child_input[0]);
    close(child_output[1]);
    signal(SIGPIPE, SIG_IGN);

    FILE *to_child = fdopen(child_input[1], "w");
    FILE *from_child = fdopen(child_output[0], "r");

    if (to_child == NULL || from_child == NULL) {
        host_error("cannot open Kitten transport streams");
        kill(child, SIGTERM);
        waitpid(child, NULL, 0);
        cleanup_runtime(runtime);
        return 1;
    }

    fprintf(to_child, "ELF-OF-FORTUNE/1\n%d\n%" PRIu64 "\n%d\n%s\n%d\n", isatty(STDOUT_FILENO) ? 1 : 0, entropy_seed(), input.ok ? 1 : 0, input.ok ? "" : input.error, argc - 1);

    for (int i = 1; i < argc; ++i) fprintf(to_child, "%s\n", argv[i]);
    if (input.ok) send_hex(to_child, input.bytes, input.size);

    fputc('\n', to_child);
    fclose(to_child);

    int relay_result = relay_output(from_child);

    fclose(from_child);

    int child_status = 0;

    while (waitpid(child, &child_status, 0) < 0 && errno == EINTR) {}

    if (relay_result < 0 || !WIFEXITED(child_status) || WEXITSTATUS(child_status) != 0) {
        if (isatty(STDOUT_FILENO)) fputs("\033[0m", stdout);

        cleanup_runtime(runtime);
        free(input.bytes);

        return WIFEXITED(child_status) ? WEXITSTATUS(child_status) : 1;
    }

    char *response_path = joined(runtime, RESPONSE_NAME);
    FILE *response = response_path == NULL ? NULL : fopen(response_path, "r");

    if (response == NULL) {
        host_error("Kitten did not produce a native-host response");
        cleanup_runtime(runtime);
        free(response_path);
        free(input.bytes);
        return 1;
    }

    char *action = NULL;
    char *output = NULL;
    char *in_place_text = NULL;
    char *response_input = NULL;
    char *patch_offset_text = NULL;
    char *payload_hex = NULL;
    char *entry_hex = NULL;

    size_t action_cap = 0;
    size_t output_cap = 0;
    size_t in_place_cap = 0;
    size_t response_input_cap = 0;
    size_t patch_offset_cap = 0;
    size_t payload_hex_cap = 0;
    size_t entry_hex_cap = 0;

    if (getline(&action, &action_cap, response) < 0) {
        host_error("malformed Kitten response");
        fclose(response);
        cleanup_runtime(runtime);
        return 1;
    }

    trim_line(action);

    if (strcmp(action, "NONE") == 0) {
        fclose(response);
        cleanup_runtime(runtime);
        free(action);
        free(response_path);
        free(input.bytes);
        return 0;
    }

    if (strcmp(action, "WRITE") != 0 || getline(&output, &output_cap, response) < 0 || getline(&in_place_text, &in_place_cap, response) < 0 || getline(&response_input, &response_input_cap, response) < 0 || getline(&patch_offset_text, &patch_offset_cap, response) < 0 || getline(&payload_hex, &payload_hex_cap, response) < 0 || getline(&entry_hex, &entry_hex_cap, response) < 0) {
        host_error("malformed Kitten response");
        fclose(response);
        cleanup_runtime(runtime);
        return 1;
    }

    fclose(response);

    trim_line(output);
    trim_line(in_place_text);
    trim_line(response_input);
    trim_line(patch_offset_text);
    trim_line(payload_hex);
    trim_line(entry_hex);

    bool in_place = strcmp(in_place_text, "1") == 0;

    if ((!in_place && strcmp(in_place_text, "0") != 0) || !input.ok || candidate == NULL || strcmp(response_input, candidate) != 0) {
        host_error("inconsistent Kitten response");
        cleanup_runtime(runtime);
        return 1;
    }

    if (!in_place && same_existing_file(candidate, output)) {
        host_error("refusing to overwrite input without --in-place");
        cleanup_runtime(runtime);
        return 1;
    }

    size_t patch_offset = 0;
    size_t payload_size = 0;
    size_t entry_size = 0;

    unsigned char *payload = decode_hex(payload_hex, &payload_size);
    unsigned char *entry = decode_hex(entry_hex, &entry_size);

    if (!parse_size(patch_offset_text, &patch_offset) || payload == NULL || payload_size == 0 || entry == NULL || entry_size != 8 || input.size < 32 || patch_offset > input.size || payload_size > input.size - patch_offset) {
        host_error("malformed sparse patch from Kitten");
        cleanup_runtime(runtime);
        free(payload);
        free(entry);
        return 1;
    }

    unsigned char *patched = malloc(input.size);

    if (patched == NULL) {
        host_error("out of memory while applying Kitten patch");
        cleanup_runtime(runtime);
        free(payload);
        free(entry);
        return 1;
    }

    memcpy(patched, input.bytes, input.size);
    memcpy(patched + patch_offset, payload, payload_size);
    memcpy(patched + 24, entry, entry_size);

    free(payload);
    free(entry);

    if (in_place) {
        char *backup = NULL;

        if (asprintf(&backup, "%s.bak", candidate) < 0 || atomic_write_file(backup, input.bytes, input.size, input.mode) < 0) {
            host_errno("cannot create backup", backup == NULL ? "" : backup);
            free(backup);
            cleanup_runtime(runtime);
            free(patched);
            return 1;
        }

        free(backup);
    }

    if (atomic_write_file(output, patched, input.size, input.mode) < 0) {
        host_errno("cannot create output", output);
        cleanup_runtime(runtime);
        free(patched);
        return 1;
    }

    printf("Created: %s\n", output);

    free(patched);
    free(action);
    free(output);
    free(in_place_text);
    free(response_input);
    free(patch_offset_text);
    free(payload_hex);
    free(entry_hex);
    free(response_path);
    free(input.bytes);

    cleanup_runtime(runtime);

    for (size_t i = 0; i < 6; ++i) free(source_paths[i]);

    free(kitten);
    free(sources);

    return 0;
}
