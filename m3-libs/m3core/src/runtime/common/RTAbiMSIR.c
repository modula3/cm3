/* MSIR ABI link-guard marker.

   Every object emitted by the MSIR backend (MSIRToLLVM.Module, for a real
   MSIRObj object build) references the versioned symbol `m3_abi_msir_v1`.
   This file defines that symbol exactly once, and is compiled into m3core
   ONLY when m3core itself is built in MSIRObj mode (see runtime/common/
   m3makefile).

   Consequences:
     - An MSIR object cannot link against a runtime built with a different
       backend (a C-mode m3core does not compile this file, so the symbol is
       undefined -> hard link error, instead of a silent ABI mismatch).
     - An incompatible future change to the MSIR calling convention bumps the
       version (v1 -> v2) in both the backend reference and this definition, so
       objects built under the old ABI fail to link against the new runtime.

   The symbol is data (a single byte); its value is never read.  On Mach-O the
   C name `m3_abi_msir_v1` and the LLVM IR name `@m3_abi_msir_v1` both mangle to
   `_m3_abi_msir_v1`; on ELF both stay `m3_abi_msir_v1`.  They match either way. */

char m3_abi_msir_v1 = 0;
