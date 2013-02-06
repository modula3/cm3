/* MD5.H - header file for MD5C.C
 */

/* Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
rights reserved.

License to copy and use this software is granted provided that it
is identified as the "RSA Data Security, Inc. MD5 Message-Digest
Algorithm" in all material mentioning or referencing this software
or this function.

License is also granted to make and use derivative works provided
that such works are identified as "derived from the RSA Data
Security, Inc. MD5 Message-Digest Algorithm" in all material
mentioning or referencing the derived work.

RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided "as is"
without express or implied warranty of any kind.

These notices must be retained in any copies of any part of this
documentation and/or software.
 */

#ifndef _SYS_MD5_H_
#define _SYS_MD5_H_

#ifdef __cplusplus
extern "C" {
#endif

/*
 * On all platforms I can think of, including the 64-bit Alpha, the type
 * "unsigned int" is 32 bits in size.  If your platform is an exception,
 * you will need to add an ifdef for it here.
 */
#ifndef UINT32
#define UINT32	unsigned int
#endif

/* MD5 context. */
typedef struct MD5Context {
  UINT32 state[4];	/* state (ABCD) */
  UINT32 count[2];	/* number of bits, modulo 2^64 (lsb first) */
  unsigned char buffer[64];	/* input buffer */
} MD5_CTX;

/* Make this compile successfully with "gcc -traditional" */
#ifndef __STDC__
#define const	/* empty */
#endif

void   __cdecl MD5Init (MD5_CTX *);
void   __cdecl MD5Update (MD5_CTX *, const unsigned char *, unsigned int);
void   __cdecl MD5Final (unsigned char [16], MD5_CTX *);
char * __cdecl MD5End(MD5_CTX *, char *);
char * __cdecl MD5File(char *, char *);
char * __cdecl MD5Data(const unsigned char *, unsigned int, char *);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _SYS_MD5_H_ */
