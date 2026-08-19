SHELL := /bin/sh

CC ?= cc
STACK ?= stack
KITTEN_DIR ?= .deps/kitten
KITTEN_REPOSITORY ?= https://github.com/evincarofautumn/kitten.git
KITTEN_COMMIT ?= 2bbc264d7f05c4a7d7b35d06773d1ab2f0623193
KITTEN_PATCH := patches/kitten-fast-transport.patch
KITTEN_BUILD_STAMP := $(KITTEN_DIR)/.elf-of-fortune-built

KITTEN_SOURCES := src/bytes.ktn src/rng.ktn src/roulette.ktn src/elf.ktn src/terminal.ktn src/main.ktn
HOST_CFLAGS := -std=c11 -O2 -Wall -Wextra -Wpedantic -Werror
FIXTURE_DIR := build/fixtures
TEST_RESULTS_DIR := build/test-results

.PHONY: all clean distclean check fixtures test-binaries test runtime-resources

all: bin/elf-of-fortune libexec/elf-of-fortune/kitten runtime-resources

build/elf-of-fortune: native/host.c
	mkdir -p build
	$(CC) $(HOST_CFLAGS) $< -o $@

bin/elf-of-fortune: build/elf-of-fortune
	mkdir -p bin
	cp $< $@

$(KITTEN_BUILD_STAMP): $(KITTEN_PATCH)
	mkdir -p "$(dir $(KITTEN_DIR))"
	@if test ! -d "$(KITTEN_DIR)/.git"; then \
		git clone "$(KITTEN_REPOSITORY)" "$(KITTEN_DIR)"; \
		git -C "$(KITTEN_DIR)" checkout --detach "$(KITTEN_COMMIT)"; \
	fi
	@if git -C "$(KITTEN_DIR)" apply --reverse --check "$(abspath $(KITTEN_PATCH))" >/dev/null 2>&1; then \
		:; \
	else \
		git -C "$(KITTEN_DIR)" apply "$(abspath $(KITTEN_PATCH))"; \
	fi
	cd "$(KITTEN_DIR)" && $(STACK) setup
	cd "$(KITTEN_DIR)" && $(STACK) build
	touch $@

libexec/elf-of-fortune/kitten: $(KITTEN_BUILD_STAMP) Makefile
	mkdir -p libexec/elf-of-fortune
	@set -eu; \
	kitten_executable=`find "$(KITTEN_DIR)/.stack-work/install" -type f -path '*/bin/kitten' -print -quit`; \
	test -n "$$kitten_executable"; \
	cp "$$kitten_executable" $@

runtime-resources: $(KITTEN_BUILD_STAMP) $(KITTEN_SOURCES)
	mkdir -p share/elf-of-fortune
	cp "$(KITTEN_DIR)/common.ktn" share/elf-of-fortune/common.ktn
	cp $(KITTEN_SOURCES) share/elf-of-fortune/

check: libexec/elf-of-fortune/kitten runtime-resources
	Kitten_datadir="$(CURDIR)/share/elf-of-fortune" \
		libexec/elf-of-fortune/kitten --check $(KITTEN_SOURCES)

$(FIXTURE_DIR)/hello-pie: tests/fixture.c
	mkdir -p $(FIXTURE_DIR)
	$(CC) -O0 $< -o $@

$(FIXTURE_DIR)/hello-exec: tests/fixture.c
	mkdir -p $(FIXTURE_DIR)
	$(CC) -O0 -no-pie $< -o $@

fixtures: test-binaries

test: all test-binaries
	ELF_OF_FORTUNE="$(CURDIR)/bin/elf-of-fortune" \
	FIXTURE_PIE="$(CURDIR)/$(FIXTURE_DIR)/hello-pie" \
	FIXTURE_EXEC="$(CURDIR)/$(FIXTURE_DIR)/hello-exec" \
	TEST_RESULTS_DIR="$(CURDIR)/$(TEST_RESULTS_DIR)" \
		tests/test.sh

clean:
	rm -rf build bin libexec share tests/exec tests/pie tests/static tests/tmp
	find tests -maxdepth 1 -type f -name '*.doomed' -delete

distclean: clean
	rm -rf .deps
