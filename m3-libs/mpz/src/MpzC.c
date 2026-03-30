/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

#include <stdio.h>
#include <gmp.h>

static char *
format_one(const char *fmt, const mpz_t z)
{
  char *p;
  
  gmp_asprintf(&p, fmt, z);

  return p;
}

void
mpz_free_formatted(char *p)
{
  void (*freefunc) (void *, size_t);

  mp_get_memory_functions (NULL, NULL, &freefunc);

  freefunc(p, 0);
}

char *
mpz_format_decimal(const mpz_t z)
{
  return format_one("%Zd", z);
}

char *
mpz_format_hexadecimal(const mpz_t z)
{
  return format_one("%Zx", z);
}

char *
mpz_format_octal(const mpz_t z)
{
  return format_one("%Zo", z);
}

char *
mpz_format_based(const mpz_t z, int base)
{
  return mpz_get_str(NULL, base, z);
}
    
