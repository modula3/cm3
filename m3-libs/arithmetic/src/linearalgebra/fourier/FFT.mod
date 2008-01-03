MODULE FFT;

(*
http://www.capital.net/~dittmer/oberon/FFT.Mod
http://www.ulfdittmer.com/
*)

(* Fast Fourier Transform in Modula-2 by Peter Moylan in June 1995
	ported to Oberon by Ulf Dittmer (dittmer (at) capital (dot) net) in March 1996

	SlowFT and FFT8 work correctly on my example (routine DoIt).
	So does FFT3, but its results' imaginary part should be magnitudes smaller. *)

IMPORT C:=Cmplx, MathL;

(********************************************************)

PROCEDURE BitRev (num, bits : INTEGER) : INTEGER;

(* Takes the last "bits" bits of "num", and reverses their order. The legal range of "bits" is 0..15. *)

    VAR j, result : INTEGER;

BEGIN
	result := 0;
	FOR j := 1 TO bits DO
		INC(result, result);
		IF ODD(num) THEN INC(result) END;
		num := num DIV 2;
	END;
	RETURN result
END BitRev;

(********************************************************)

PROCEDURE Scramble (N, m : INTEGER;  VAR data : ARRAY OF C.COMPLEX);

(* Re-sorts a data array of size N = 2^m into bit-reversed order,
i.e. for all j data[j] is swapped with data[BitRev(j,m)].

For the moment this is a "proof of concept" version which uses a crude method for doing
the scrambling. It works, but I've found a faster method - see Shuffle below. *)

    VAR j, newpos : INTEGER;
		temp : C.COMPLEX;

BEGIN
	FOR j := 1 TO N-1 DO
	    newpos := BitRev (j, m);
	    IF newpos > j THEN
			temp := data[j];  data[j] := data[newpos]; data[newpos] := temp;
	    END;
	END;
END Scramble;

(*******************************************************
					THE SHUFFLING ALGORITHM

The following two procedures work together to put an array into "bit-reversed" order:
for all j, data[j] is swapped with data[k], where k is obtained by writing j as a binary
number and then reversing the order of its bits. The method we use here is admittedly
rather obscure - it was derived by a succession of program transformations starting
from a more readable but highly recursive algorithm - but it has a significant speed
advantage over more obvious ways of doing the job.
*******************************************************)

PROCEDURE Commit (start, step, size : INTEGER; VAR data : ARRAY OF C.COMPLEX);

(* Swaps all elements in seq with their bit-reversed partners. The difference between this
procedure and Shuffle is that here we are sure that every element in seq will be involved in a
swap, whereas in Shuffle we are dealing with a sequence where some elements will stay in place.

The sequence seq is defined to be the sequence of size elements whose first element is at
location (start + step DIV 2), and subsequent elements occur at subscript increments of step. *)

    VAR N, destination, depth, oldsize : INTEGER;
		Stack : ARRAY 13 OF INTEGER;
		temp : C.COMPLEX;

BEGIN
	N := step*size;
	destination := start + N;
	INC (start, step DIV 2);
	depth := 0;

	LOOP
	    (* Swap one pair of elements. *)

	    temp := data[start];
	    data[start] := data[destination];
	    data[destination] := temp;

	    (* Find the next subsequence to work on. *)

	    INC (depth);
	    oldsize := size;
	    N := N DIV size;
	    step := step*size;
	    size := 1;

		LOOP
			IF oldsize > 1 THEN EXIT END;

			DEC (depth);
			IF depth = 0 THEN RETURN END;
	
			oldsize := Stack[depth];
			DEC (destination, N);
			step := step DIV 2;
			DEC (start, step);
			N := 2*N;
			size := 2*size
	    END;

	    INC (start, step DIV 2);
	    INC (destination, N);
	    Stack[depth] := oldsize DIV 2
	END
END Commit;

(************************************************************************)

PROCEDURE Shuffle (size : INTEGER;  VAR data : ARRAY OF C.COMPLEX);

(* Shuffles an array into bit-reversed order. The actual work of moving subsequences is done
by procedure Commit. The present procedure has the job of working out which subsequences
to pass on to Commit. This culling procedure means that we step right past elements that
don't need to be moved, and that those that do need to be moved are moved exactly once. *)

    VAR start, step, newstep, level, scale : INTEGER;
		Stack : ARRAY 8 OF INTEGER;

	(* The upper stack bound needs to be Log2(size) DIV 2. *)

    BEGIN
	start := 0;  step := 1;  level := 0;

	LOOP
	    IF size < 4 THEN
		IF level <= 1 THEN
		    RETURN;
		END;
		scale := 2*Stack[level];
		newstep := step DIV scale;
		INC (start, 2*(size-1)*step + 3*(step + newstep) DIV 2);
		step := newstep;
		size := scale*scale*size;
		DEC (level);
		Stack[level] := 2*Stack[level];
	    ELSE
		INC (level);  Stack[level] := 1;
	    END;

	    step := 2*step;
	    size := size DIV 4;

	    (* For the current subsequence, swap all odd elements in *)
	    (* the first half with even elements in the second half. *)

	    Commit (start, step, size, data);

	END;

    END Shuffle;

(************************************************
			   " DECIMATION IN TIME " ALGORITHM (Cooley-Tukey)
************************************************)

PROCEDURE FFTDTSN (Direct : BOOLEAN; N : INTEGER; VAR data : ARRAY OF C.COMPLEX);

(* Fast Fourier transform of an array of N complex data points. The result is returned in the
same array. N must be a power of two. The algorithm is essentially a form of one of the
Cooley-Tukey transforms, but I've set up the computations in such a way that there are no
sine/cosine calculations. This should, I think, improve both speed and accuracy.
This version assumes scrambled input data. *)

    VAR gap, j, k, pos1, pos2, blocksize, groups : INTEGER;
		temp1, temp2, multiplier, mstep : C.COMPLEX;
		scale : LONGREAL;

BEGIN
	(* For extra speed, we pull out the first pass as a special case. *)
	IF N >= 2 THEN
		FOR pos1 := 0 TO N-2 BY 2 DO
			temp1 := data[pos1];
			temp2 := data[pos1+1];
			C.Add(temp1, temp2, data[pos1]);
			C.Sub(temp1, temp2, data[pos1+1])
		END
	END;

	(* Now for the second and subsequent passes. *)
	groups := N DIV 4;  gap := 2;
	IF Direct THEN
	    C.Make(0.0, -1.0, mstep)
	ELSE
	    C.Make(0.0, +1.0, mstep)
	END;

	WHILE groups >= 1 DO
	    C.Make (1.0, 0.0, multiplier);
	    blocksize := 2*gap;
	    FOR j := 0 TO gap-1 DO
			pos1 := j;
			pos2 := pos1 + gap;
			FOR k := 0 TO groups-1 DO
			    temp1 := data[pos1];
			    C.Mul(multiplier, data[pos2], temp2);
			    C.Add(temp1, temp2, data[pos1]);
			    C.Sub(temp1, temp2, data[pos2]);
			    INC (pos1, blocksize);
			    INC (pos2, blocksize)
			END;
			C.Mul(multiplier, mstep, multiplier)
	    END;
	    groups := groups DIV 2;
		C.Sqrt(mstep, mstep);
	    gap := blocksize
	END;

	(* For an inverse transform, the results have to be scaled by a factor 1/N. *)
	IF ~Direct THEN
	    scale := 1.0 / N;
	    FOR k := 0 TO N-1 DO
			C.RealMul (scale, data[k], data[k])
	    END
	END
END FFTDTSN;

(********************************************************)

PROCEDURE FFT3* (Direct : BOOLEAN; N : INTEGER; VAR data: ARRAY OF C.COMPLEX);

    (* Fast Fourier transform of an array of N complex data points. *)
    (* The result is returned in the same array. N must be a power of two. *)

BEGIN
	Shuffle (N, data);
	FFTDTSN (Direct, N, data)
END FFT3;

(********************************************************
				" DECIMATION IN FREQUENCY " ALGORITHM (Sande-Tukey)
********************************************************)

PROCEDURE FFT8G (Direct : BOOLEAN; N : INTEGER; VAR data : ARRAY OF C.COMPLEX);

(* Fast Fourier transform of an array of N complex data points. The result is returned in the
same array. N must be a power of two. The output is in scrambled order. *)

    VAR k, pos1, pos2, g, groups, halfN : INTEGER;
		scale, temp1, temp2, temp3, WW : C.COMPLEX;
		theta : LONGREAL;

BEGIN
	IF N <= 1 THEN RETURN END(*IF*);
	groups := 1;  halfN := N DIV 2;
	theta := MathL.pi / halfN;
	IF Direct THEN
	    C.Make(MathL.cos(theta), -MathL.sin(theta), WW)
	ELSE
	    C.Make(MathL.cos(theta), MathL.sin(theta), WW)
	END;
	WHILE N > 1 DO
	    C.Make (1.0, 0.0, scale);
	    FOR k := 0 TO halfN-1 DO
			pos1 := k;
			FOR g := 0 TO groups-1 DO
			    pos2 := pos1 + halfN;
			    temp1 := data[pos1];
			    temp2 := data[pos2];
			    C.Add (temp1, temp2, data[pos1]);
			    C.Sub (temp1, temp2, temp3);
			    C.Mul (scale, temp3, data[pos2]);
			    INC (pos1, N);
			END;
			C.Mul (scale, WW, scale)
	    END;
	    groups := 2*groups;  N := halfN;  halfN := N DIV 2;
	    C.Mul (WW, WW, WW)
	END;

	(* For an inverse transform, the results have to be scaled by *)
	(* a factor 1/N. By now groups = original N. *)

	IF ~Direct THEN
	    theta := 1.0 / groups;
	    FOR k := 0 TO groups-1 DO
			C.RealMul (theta, data[k], data[k])
	    END
	END
END FFT8G;

(********************************************************)

PROCEDURE FFT8* (Direct : BOOLEAN; N : INTEGER; VAR data : ARRAY OF C.COMPLEX);

(* Fast Fourier transform of an array of N complex data points. The result is returned in the
same array. N must be a power of two. This is based on the Sande variant of the FFT. *)

    BEGIN
		FFT8G (Direct, N, data);
		Shuffle (N, data)
    END FFT8;

(******************************************************
			NAIVE FORM OF THE DISCRETE FOURIER TRANSFORM
******************************************************)

PROCEDURE SlowFT* (Direct : BOOLEAN; N : INTEGER; data : ARRAY OF C.COMPLEX; VAR res : ARRAY OF C.COMPLEX);

    VAR r, k : INTEGER;
		temp1 : LONGREAL;
		sum, power, temp2, weight : C.COMPLEX;

BEGIN
	FOR r := 0 TO N-1 DO
		C.Make(0.0, 0.0, sum);
		FOR k := 0 TO N-1 DO
			temp1 := 2.0 * MathL.pi * r * k / N;
			IF Direct THEN
				C.Make (0.0, -temp1, power)
			ELSE
				C.Make (0.0, temp1, power)
			END;
			C.Exp (power, weight);
			C.Mul (weight, data[k], temp2);
			C.Add (sum, temp2, sum)
		END;
		temp1 := 1.0 / N;
		IF Direct THEN
			res[r] := sum
		ELSE
			C.RealMul (temp1, sum, res[r])
		END
	END
END SlowFT;

(**********   Example : Multiplication of two polynomials   **********)

PROCEDURE DoIt*;
	VAR one, two, three, four, five, six : ARRAY 8 OF C.COMPLEX;
		loop : INTEGER;
BEGIN
	C.Make(1,0,one[0]);	C.Make(2,0,one[1]);	C.Make(3,0,one[2]);	C.Make(4,0,one[3]);
	C.Make(-4,0,two[0]);	C.Make(3,0,two[1]);	C.Make(-2,0,two[2]);	C.Make(1,0,two[3]);

(* (1 + 2*x + 3*x^2 + 4*x^3) * (-4 + 3*x - 2*x^2 + x^3) = 
			-4 - 5*x - 8*x^2 - 10*x^3 + 8*x^4 - 5*x^5 + 4*x^6 *)

	FOR loop := 4 TO 7 DO
		C.Make(0.0, 0.0, one[loop]);
		C.Make(0.0, 0.0, two[loop])
	END;

(* test SlowFT *)

	SlowFT(TRUE, 8, one, three);
	SlowFT(TRUE, 8, two, four);
	FOR loop := 0 TO 7 DO
		C.Mul(three[loop], four[loop], five[loop])
	END;
	SlowFT(FALSE, 8, five, six);
	FOR loop := 0 TO 7 DO C.Write(six[loop], 10) END;

(* test FFT8 or FFT3 *)
(* note that FFT3 and FFT8 scramble their input array while SlowFT doesn't *)
(*
	FFT3(TRUE, 8, one);
	FFT3(TRUE, 8, two);
	FOR loop := 0 TO 7 DO
		C.Mul(one[loop], two[loop], five[loop])
	END;
	FFT3(FALSE, 8, five);
	FOR loop := 0 TO 7 DO C.Write(five[loop], 10) END;
*)
END DoIt;

END FFT.DoIt		<-- This is an executable command.
