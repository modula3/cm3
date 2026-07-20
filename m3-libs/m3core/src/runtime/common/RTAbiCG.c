/* CG (C / native) ABI link-guard marker — mirror of RTAbiMSIR.c.

   On the ex_stack targets where the MSIR backend can coexist with the C
   backend, every C-backend object references `m3_abi_cg_v1` (see M3C.m3's
   Prefix_Print).  This file defines that symbol, and is compiled into m3core
   whenever m3core is NOT built in MSIRObj mode (see runtime/common/m3makefile).

   Consequence: a CG object cannot link against an MSIR-built runtime (which
   defines `m3_abi_msir_v1` but not `m3_abi_cg_v1`) — undefined symbol at link
   instead of a silent ABI mismatch.  Together with RTAbiMSIR.c this makes both
   mismatch directions hard link errors.

   Defined unconditionally here (the reference is what is target-scoped in
   M3C.m3); an unreferenced definition on a non-MSIR target is harmless.  The
   symbol is data; its value is never read. */

char m3_abi_cg_v1 = 0;
