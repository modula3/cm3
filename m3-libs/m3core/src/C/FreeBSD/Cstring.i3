(* Copyright (C) 1989, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* File: Cstring.i3                                           *)
(* Last modified on Mon Oct 17 09:19:10 PDT 1994 by kalsow    *)
(*      modified on Tue Apr 20 20:16:18 PDT 1993 by muller    *)
(*      modified on Sat Jan 20 22:31:44 1990 by jerome        *)
(*      Olaf Wagner 16.09.1994                                *)


INTERFACE Cstring;

FROM Ctypes IMPORT char_star, const_char_star, const_void_star,
                   int, void_star, unsigned_long;

TYPE
  size_t =  unsigned_long;

<*EXTERNAL*> 
PROCEDURE memchr (s: const_void_star; c: int; n: size_t): void_star;

<*EXTERNAL*>
PROCEDURE memcpy (s1: void_star; s2: const_void_star; n: size_t): void_star;

<*EXTERNAL*> 
PROCEDURE memset (s: void_star; c: int; n: size_t): void_star;

<*EXTERNAL*>
PROCEDURE memcmp (s1: const_void_star; s2: const_void_star; n: size_t): int;

<*EXTERNAL*>
PROCEDURE strcpy (s1: char_star; s2: const_char_star): char_star;

<*EXTERNAL*>
PROCEDURE strncpy (s1: char_star; s2: const_char_star; n: size_t): char_star;

<*EXTERNAL*>
PROCEDURE strcat (s1: char_star; s2: const_char_star): char_star;

<*EXTERNAL*>
PROCEDURE strncat  (s1: char_star; s2: const_char_star; n: size_t): char_star;

<*EXTERNAL*>
PROCEDURE strchr (s: const_char_star; c: int): char_star;

<*EXTERNAL*>
PROCEDURE strrchr (s: const_char_star; c: int): char_star;

<*EXTERNAL*>
PROCEDURE strpbrk (s1: const_char_star; s2: const_char_star): char_star;

<*EXTERNAL*>
PROCEDURE strtok (s1: char_star; s2: const_char_star): char_star;

<*EXTERNAL*>
PROCEDURE strcmp (s1: const_char_star; s2: const_char_star): int;

<*EXTERNAL*>
PROCEDURE strncmp (s1: const_char_star; s2: const_char_star; n: size_t): int;

<*EXTERNAL*>
PROCEDURE strlen (s: const_char_star): size_t;

<*EXTERNAL*>
PROCEDURE strspn (s1: const_char_star; s2: const_char_star): int;

<*EXTERNAL*>
PROCEDURE strcspn (s1: const_char_star; s2: const_char_star): size_t;

<*EXTERNAL*>
PROCEDURE memmove (s1: void_star; s2: const_void_star; n: size_t): void_star;

<*EXTERNAL*>
PROCEDURE strcoll (s1: const_char_star; s2: const_char_star): int;

(*
<*EXTERNAL*>
PROCEDURE strxrfm (s1: char_star; s2: const_char_star; n: size_t): size_t;
*)

<*EXTERNAL*>
PROCEDURE strstr (s1: const_char_star; s2: const_char_star): char_star;

<*EXTERNAL*>
PROCEDURE strerror (errnum: int): char_star;

END Cstring.

