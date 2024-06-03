/*
  Glue to fix broken prototypes in CM3-d5.11.9

  Author : Mika Nystrom <mika.nystroem@intel.com>
  June, 2024

  Copyright (c) Intel Corporation, 2024.  All rights reserved.

  See COPYRIGHT-INTEL for terms.  

  SPDX-License-Identifier: BSD-3-Clause

 */

#include <math.h>

double
MathPosixC__frexp_result_glue(double x)
{
  int dummy;
  
  return frexp(x, &dummy);
}

int
MathPosixC__frexp_exp_glue(double x)
{
  int exp;

  (void)frexp(x, &exp);

  return exp;
}

double
MathPosixC__modf_result_glue(double x)
{
  double dummy;
  
  return modf(x, &dummy);
}

double
MathPosixC__modf_intpart_glue(double x)
{
  double intpart;

  (void)modf(x, &intpart);

  return intpart;
}



