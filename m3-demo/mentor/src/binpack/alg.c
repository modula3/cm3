(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

@FirstFit
void first_fit (B, N)@
int B, N;
{
    int i, b, bin;
    float weight, *bins, lost = 0.0;

    @1 bins = (float *) malloc(B * sizeof (float));@
    @2 for (b = 0; b < B; b++) bins[b] := 0.0;@
    @3 for (i = 0; i < N; i++) {@
        @4 weight = random_float () / 2.0;@
        for (@5 bin = 0@; 
             @6 (bin < B) && (bins[bin] + weight > 1.0)@; 
             @7 bin++@) /*EMPTY*/;
        @8 if (bin = B) @
            @9 lost += weight;@
        else
            @10 bins[bin] += weight;@
    }
}
@FirstFit
