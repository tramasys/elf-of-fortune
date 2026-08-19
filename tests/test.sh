#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
TOOL=${ELF_OF_FORTUNE:-"$ROOT/bin/elf-of-fortune"}
FIXTURE_DIR=${FIXTURE_DIR:-"$ROOT/build/fixtures"}
PIE=${FIXTURE_PIE:-"$FIXTURE_DIR/hello-pie"}
EXEC=${FIXTURE_EXEC:-"$FIXTURE_DIR/hello-exec"}
TMP=${TEST_RESULTS_DIR:-"$ROOT/build/test-results"}
CC=${CC:-cc}

fail() {
    echo "FAIL: $*" >&2
    exit 1
}

assert_contains() {
    local file=$1 expected=$2
    grep -Fq -- "$expected" "$file" ||
        fail "expected '$expected' in $file"
}

expect_failure() {
    local file=$1 expected=$2
    shift 2
    if "$TOOL" --no-anim --dry-run "$file" >"$TMP/invalid.out" 2>"$TMP/invalid.err"; then
        fail "accepted invalid fixture $file"
    fi
    assert_contains "$TMP/invalid.err" "$expected"
}

mkdir -p "$FIXTURE_DIR"
if [[ ! -x $PIE || $ROOT/tests/fixture.c -nt $PIE ]]; then
    "$CC" -O0 "$ROOT/tests/fixture.c" -o "$PIE"
fi
if [[ ! -x $EXEC || $ROOT/tests/fixture.c -nt $EXEC ]]; then
    "$CC" -O0 -no-pie "$ROOT/tests/fixture.c" -o "$EXEC"
fi
if [[ -d $TMP ]]; then
    find "$TMP" -mindepth 1 -delete
fi
mkdir -p "$TMP"
ulimit -c 0

[[ $("$PIE") == "still alive" ]] || fail "original PIE failed"
[[ $("$EXEC") == "still alive" ]] || fail "original ET_EXEC failed"
cp "$EXEC" "$TMP/original-exec"

declare -A SIGNAL_STATUS=(
    [UD2]=132 [INT3]=133 [HLT]=139 [ICEBP]=133 [UD1]=132 ["LOCK NOP"]=132
)
declare -A PAYLOAD=(
    [UD2]=0f0b [INT3]=cc [HLT]=f4 [ICEBP]=f1 [UD1]=0fb9c0 ["LOCK NOP"]=f090
)

for instruction in UD2 INT3 HLT ICEBP UD1 "LOCK NOP"; do
    slug=${instruction// /_}
    dead="$TMP/$slug.dead"
    report="$TMP/$slug.report"
    "$TOOL" --seed 123 --no-anim --instruction "$instruction" \
        -o "$dead" "$EXEC" >"$report"
    readelf -h "$dead" >/dev/null

    offset=$(awk '/^File offset:/ { sub(/^0x/, "", $3); print $3 }' "$report")
    address=$(awk '/^Address:/ { sub(/^0x/, "", $2); print $2 }' "$report")
    entry=$(readelf -h "$dead" | awk '/Entry point address:/ { sub(/^0x/, "", $4); print $4 }')
    (( 16#$entry == 16#$address )) || fail "$instruction entry point mismatch"

    actual=$(xxd -p -l $((${#PAYLOAD[$instruction]} / 2)) \
        -s "$((16#$offset))" "$dead")
    [[ $actual == "${PAYLOAD[$instruction]}" ]] || fail "$instruction payload mismatch"

    exec_load=$(readelf -W -l "$dead" | awk '$1 == "LOAD" && $0 ~ /R E/ { print $3, $5; exit }')
    read -r load_address load_size <<<"$exec_load"
    (( 16#$address >= 16#${load_address#0x} )) || fail "$instruction entry below executable LOAD"
    (( 16#$address < 16#${load_address#0x} + 16#${load_size#0x} )) ||
        fail "$instruction entry outside executable LOAD"

    set +e
    ("$dead" >/dev/null 2>&1) 2>/dev/null
    result=$?
    set -e
    [[ $result -eq ${SIGNAL_STATUS[$instruction]} ]] ||
        fail "$instruction exited $result, expected ${SIGNAL_STATUS[$instruction]}"

    patch_length=$((${#PAYLOAD[$instruction]} / 2))
    while read -r position _; do
        zero_based=$((position - 1))
        if ! (( (zero_based >= 24 && zero_based < 32) ||
                (zero_based >= 16#$offset && zero_based < 16#$offset + patch_length) )); then
            fail "$instruction changed unexpected byte at 0x$(printf '%x' "$zero_based")"
        fi
    done < <(cmp -l "$EXEC" "$dead" || true)
done

# PIE support and deterministic selection.
"$TOOL" --seed 999 --no-anim --instruction UD2 -o "$TMP/pie.dead" \
    "$PIE" >"$TMP/pie.report"
set +e
("$TMP/pie.dead" >/dev/null 2>&1) 2>/dev/null
pie_result=$?
set -e
[[ $pie_result -eq 132 ]] || fail "patched PIE did not die from SIGILL"

"$TOOL" --seed 42 --no-anim -o "$TMP/deterministic-a" "$EXEC" >/dev/null
"$TOOL" --seed 42 --no-anim -o "$TMP/deterministic-b" "$EXEC" >/dev/null
cmp "$TMP/deterministic-a" "$TMP/deterministic-b" >/dev/null ||
    fail "same seed did not reproduce output"

# Dry runs and default output leave the victim alone.
cp "$EXEC" "$TMP/dry-victim"
"$TOOL" --seed 7 --no-anim --dry-run "$TMP/dry-victim" >"$TMP/dry.report"
[[ ! -e "$TMP/dry-victim.doomed" ]] || fail "dry run wrote an output"
cmp "$EXEC" "$TMP/dry-victim" >/dev/null || fail "dry run changed input"

# In-place mode backs up first and preserves the executable mode.
cp "$EXEC" "$TMP/in-place"
chmod 751 "$TMP/in-place"
"$TOOL" --seed 7 --no-anim --instruction UD2 --in-place "$TMP/in-place" >/dev/null
cmp "$TMP/in-place.bak" "$EXEC" >/dev/null || fail "bad in-place backup"
[[ $(stat -c %a "$TMP/in-place") == 751 ]] || fail "executable mode was not preserved"

# Missing section headers use the executable PT_LOAD fallback.
cp "$EXEC" "$TMP/no-sections"
printf '\0\0\0\0\0\0\0\0' | dd of="$TMP/no-sections" bs=1 seek=$((0x28)) conv=notrunc status=none
printf '\0\0' | dd of="$TMP/no-sections" bs=1 seek=$((0x3c)) conv=notrunc status=none
"$TOOL" --seed 3 --no-anim --dry-run "$TMP/no-sections" >"$TMP/segment.report"
assert_contains "$TMP/segment.report" "Region:       segment:PT_LOAD#"

# Invalid inputs fail cleanly.
dd if=/dev/null of="$TMP/empty" status=none
printf 'this is plainly not an ELF file\n' >"$TMP/text"
dd if="$EXEC" of="$TMP/truncated" bs=1 count=20 status=none
cp "$EXEC" "$TMP/elf32"
printf '\1' | dd of="$TMP/elf32" bs=1 seek=4 conv=notrunc status=none
cp "$EXEC" "$TMP/wrong-arch"
printf '\50\0' | dd of="$TMP/wrong-arch" bs=1 seek=$((0x12)) conv=notrunc status=none
cp "$EXEC" "$TMP/bad-phoff"
printf '\377\377\377\377\377\377\377\177' | dd of="$TMP/bad-phoff" bs=1 seek=$((0x20)) conv=notrunc status=none
cp "$EXEC" "$TMP/bad-shoff"
printf '\377\377\377\377\377\377\377\177' | dd of="$TMP/bad-shoff" bs=1 seek=$((0x28)) conv=notrunc status=none
cp "$EXEC" "$TMP/shared-library"
printf '\3\0' | dd of="$TMP/shared-library" bs=1 seek=$((0x10)) conv=notrunc status=none
printf '\0\0\0\0\0\0\0\0' | dd of="$TMP/shared-library" bs=1 seek=$((0x18)) conv=notrunc status=none

shoff=$(readelf -h "$EXEC" | awk '/Start of section headers:/ { print $5 }')
text_index=$(readelf -W -S "$EXEC" | sed -n 's/.*\[ *\([0-9][0-9]*\)\] \.text .*/\1/p')
cp "$EXEC" "$TMP/oversized-section"
printf '\377\377\377\177' | dd of="$TMP/oversized-section" bs=1 \
    seek=$((shoff + text_index * 64 + 36)) conv=notrunc status=none

expect_failure "$TMP/empty" "truncated ELF header"
expect_failure "$TMP/text" "not an ELF file"
expect_failure "$TMP/truncated" "truncated ELF header"
expect_failure "$TMP/elf32" "ELF32 is not supported"
expect_failure "$TMP/wrong-arch" "architecture is not x86-64"
expect_failure "$TMP/bad-phoff" "malformed ELF"
expect_failure "$TMP/bad-shoff" "malformed ELF"
expect_failure "$TMP/shared-library" "ELF appears to be a shared library"
expect_failure "$TMP/oversized-section" "malformed ELF"

"$TOOL" --help >"$TMP/help"
assert_contains "$TMP/help" "elf-of-fortune [options] <input>"
cmp "$TMP/original-exec" "$EXEC" >/dev/null ||
    fail "normal operation modified the original executable"

echo "All ELF-of-Fortune tests passed."
