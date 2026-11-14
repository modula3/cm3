# 128-bit EXTENDED on AMD64_LINUX

On AMD64_LINUX, M3C makes EXTENDED a 128-bit float: it emits
`typedef _Float128 EXTENDED;`. That breaks in two ways.

## Problems

1. **C++ compile failure.** M3C output is compiled as C++. g++ < 13 (e.g. g++ 12 on
   Debian 12) and every clang++ reject `_Float128` in C++, so a stock install fails
   while rebuilding m3core.
2. **Wrong EXTENDED size when cross-compiling.** `Target.Extended` starts from the
   compiler's own host size, and `Target.Init` reset it only for targets other than
   AMD64_LINUX. A cm3 built on a host with a 64-bit EXTENDED (e.g. ARM64_DARWIN)
   therefore compiled AMD64_LINUX code with a 64-bit EXTENDED, and m3core's ext128
   LOOPHOLEs failed.

## Changes

| Area | File | Change |
|---|---|---|
| m3middle | `m3-sys/m3middle/src/Target.m3` | Set 128-bit EXTENDED for AMD64_LINUX on any host. |
| m3back | `m3-sys/m3back/src/M3C.m3` | Before the typedef, emit `#if defined(__cplusplus) && defined(__x86_64__)` / `#define _Float128 __float128` / `#endif`. `__float128` is the same IEEE binary128 type on x86-64, and the type libquadmath uses. This is the real fix. |
| cminstall | `m3-sys/cminstall/src/config/AMD64_LINUX` | Probe `g++` for `_Float128`; if it is missing, add `-D_Float128=__float128` to `SYSTEM_CC` and `SYSTEM_CXXC`. |

The cminstall change stays until a new boot archive exists. The bootstrap and the
first full-upgrade stage run the old M3C, which lacks the mapping. Once a boot
archive carries the m3back fix, the config probe can be removed.

`__STDCPP_FLOAT128_T__` would not work as the guard: it is defined only in C++23 mode.

## Verification

- **Probe:** bare `_Float128` fails to compile as C++ on g++ 12 and on every clang++
  tested. With the mapping it compiles on g++ 12, 14 and 16 and on clang++.
- **End-to-end on debian:12 (g++ 12):** a full upgrade with the cminstall workaround
  removed rebuilt m3core and libm3, and a program using EXTENDED arithmetic printed
  the expected result.
- **Caveat:** cm3 deletes the generated C, so it was not inspected directly. The proof
  that the M3C prefix is what fixed the build is by elimination.

## Out of scope

The gold-linker fix (`Linux.common` forcing `-fuse-ld=gold`, which fails where
binutils ships without `ld.gold`, e.g. Fedora 44) is unrelated and goes in a
separate change.
