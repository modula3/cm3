GENERIC INTERFACE FourierTransform(C);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Fast Fourier Transforms (FFT's)

   Much more sophisticated routines can be found in the Modula-3 interface
   to the FFTW library.

   3/18/96 Warren Smith Initial version *)

(*==========================*)
(**************************************
Reorders array a[0..n-1] so that element[i] is
swapped with element[reverse-bit-order[i]].
Two successive calls are identity.
The algorithm below runs in O(n) time and is based on implementing a
reverse-binary counter. The forward counter f increments 0..n/2-1 step 2
and if r, 0<=r<n/2 is the bit-reverse of f,
we swap(f+1,n/2+r) with no test needed and we do
swap(f,r) if r>f and also swap(n-1-f,n-1-r) if f,r both <n/2.
***************************************************)

PROCEDURE ReOrder (VAR a: ARRAY OF C.T);




(***************************************
 direction = +1 for inverse FFT, -1 for forward.  (See FFT defn
in module.)

 a[] overwritten by transform.
NOTE: You must call ReOrder(a) before calling this routine,
because this routine assumes it has re-ordered a[]s as its input.
To do an FFT of some data, therefore, we would call
  ReOrder(data); FFTwithWrongOrderedInput(data);
I have separated the routines this way because I want to be able to
avoid the ReOrder when computing convolutions and correlations.
****************************************)
PROCEDURE FFTwithWrongOrderedInput (VAR a        : ARRAY OF C.T;
                                        direction: [-1 .. 1]     );



(*----------------------------*)
PROCEDURE Test ();
(*perform assertions*)
(*==========================*)
END FourierTransform.
