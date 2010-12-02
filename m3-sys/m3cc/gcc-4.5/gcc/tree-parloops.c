/* Modula-3: modified */

/* Loop autoparallelization.
   Copyright (C) 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Sebastian Pop <pop@cri.ensmp.fr> and
   Zdenek Dvorak <dvorakz@suse.cz>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tree-flow.h"
#include "cfgloop.h"
#include "ggc.h"
#include "tree-data-ref.h"
#include "diagnostic.h"
#include "tree-pass.h"
#include "tree-scalar-evolution.h"
#include "hashtab.h"
#include "langhooks.h"
#include "tree-vectorizer.h"

#ifdef __cplusplus
extern "C" {
#endif

/* This pass tries to distribute iterations of loops into several threads.
   The implementation is straightforward -- for each loop we test whether its
   iterations are independent, and if it is the case (and some additional
   conditions regarding profitability and correctness are satisfied), we
   add GIMPLE_OMP_PARALLEL and GIMPLE_OMP_FOR codes and let omp expansion
   machinery do its job.

   The most of the complexity is in bringing the code into shape expected
   by the omp expanders:
   -- for GIMPLE_OMP_FOR, ensuring that the loop has only one induction
      variable and that the exit test is at the start of the loop body
   -- for GIMPLE_OMP_PARALLEL, replacing the references to local addressable
      variables by accesses through pointers, and breaking up ssa chains
      by storing the values incoming to the parallelized loop to a structure
      passed to the new function as an argument (something similar is done
      in omp gimplification, unfortunately only a small part of the code
      can be shared).

   TODO:
   -- if there are several parallelizable loops in a function, it may be
      possible to generate the threads just once (using synchronization to
      ensure that cross-loop dependences are obeyed).
   -- handling of common scalar dependence patterns (accumulation, ...)
   -- handling of non-innermost loops  */

/*
  Reduction handling:
  currently we use vect_is_simple_reduction() to detect reduction patterns.
  The code transformation will be introduced by an example.


parloop
{
  int sum=1;

  for (i = 0; i < N; i++)
   {
    x[i] = i + 3;
    sum+=x[i];
   }
}

gimple-like code:
header_bb:

  # sum_29 = PHI <sum_11(5), 1(3)>
  # i_28 = PHI <i_12(5), 0(3)>
  D.1795_8 = i_28 + 3;
  x[i_28] = D.1795_8;
  sum_11 = D.1795_8 + sum_29;
  i_12 = i_28 + 1;
  if (N_6(D) > i_12)
    goto header_bb;


exit_bb:

  # sum_21 = PHI <sum_11(4)>
  printf (&"%d"[0], sum_21);


after reduction transformation (only relevant parts):

parloop
{

....


  # Storing the initial value given by the user.  #

  .paral_data_store.32.sum.27 = 1;

  #pragma omp parallel num_threads(4)

  #pragma omp for schedule(static)

  # The neutral element corresponding to the particular
  reduction's operation, e.g. 0 for PLUS_EXPR,
  1 for MULT_EXPR, etc. replaces the user's initial value.  #

  # sum.27_29 = PHI <sum.27_11, 0>

  sum.27_11 = D.1827_8 + sum.27_29;

  GIMPLE_OMP_CONTINUE

  # Adding this reduction phi is done at create_phi_for_local_result() #
  # sum.27_56 = PHI <sum.27_11, 0>
  GIMPLE_OMP_RETURN

  # Creating the atomic operation is done at
  create_call_for_reduction_1()  #

  #pragma omp atomic_load
  D.1839_59 = *&.paral_data_load.33_51->reduction.23;
  D.1840_60 = sum.27_56 + D.1839_59;
  #pragma omp atomic_store (D.1840_60);

  GIMPLE_OMP_RETURN

 # collecting the result after the join of the threads is done at
  create_loads_for_reductions().
  The value computed by the threads is loaded from the
  shared struct.  #


  .paral_data_load.33_52 = &.paral_data_store.32;
  sum_37 =  .paral_data_load.33_52->sum.27;
  sum_43 = D.1795_41 + sum_37;

  exit bb:
  # sum_21 = PHI <sum_43, sum_26>
  printf (&"%d"[0], sum_21);

...

}

*/

/* Detect parallel loops and generate parallel code using libgomp
   primitives.  Returns true if some loop was parallelized, false
   otherwise.  */

bool
parallelize_loops (void)
{
  return false;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
