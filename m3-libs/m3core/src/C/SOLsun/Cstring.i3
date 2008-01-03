(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Cstring.i3                                            *)
(* Last modified on Fri Sep 10 14:32:27 PDT 1993 by muller         *)
(*      modified on Sat Jan 20 22:31:44 1990 by jerome         *)


INTERFACE Cstring;

FROM Ctypes IMPORT char_star, const_char_star, const_void_star,
                   int, void_star;


TYPE
  size_t            =  int;



(* From:  /usr/include/memory.h

DESCRIPTION
     These functions operate efficiently on memory areas (arrays
     of characters bounded by a count, not terminated by a null
     character).  They do not check for the overflow of any
     receiving memory area.
*)


(*   The memchr subroutine returns a pointer to the first
     occurrence of character c in the first n characters of
     memory area s, or a NULL pointer if c does not occur.  *)

<*EXTERNAL*> PROCEDURE memchr   (s: const_void_star; c: int; n: size_t): void_star;


(*   The memcpy subroutine copies n characters from memory area
     s2 to s1.  It returns s1.  *)

<*EXTERNAL*> PROCEDURE memcpy   (s1: void_star; s2: const_void_star; n: size_t): void_star;


(*   The memset subroutine sets the first n characters in memory
     area s to the value of character c.  It returns s.  *)

<*EXTERNAL*> PROCEDURE memset   (s: void_star; c: int; n: size_t): void_star;


(*   The memcmp subroutine compares its arguments, looking at the
     first n characters only, and returns an integer less than,
     equal to, or greater than 0, according as s1 is lexicograph-
     ically less than, equal to, or greater than s2.  *)

<*EXTERNAL*> PROCEDURE memcmp   (s1: const_void_star; s2: const_void_star; n: size_t): int;



(* From:  /usr/include/string.h

DESCRIPTION
     The arguments s1, s2, and s point to strings (arrays of
     characters terminated by a null character).  The functions
     strcat, strncat, strcpy, and strncpy subroutines all alter
     s1. These functions do not check for overflow of the array
     pointed to by s1.
*)



(*   The strcpy subroutine copies string s2 to s1, stopping after
     the null character has been copied.  The strncpy subroutine
     copies exactly n characters, truncating s2 or adding null
     characters to s1 if necessary.  The result will not be
     null-terminated if the length of s2 is n or more.  Each
     function returns s1.  *)

<*EXTERNAL*> PROCEDURE strcpy   (s1: char_star; s2: const_char_star): char_star;
<*EXTERNAL*> PROCEDURE strncpy  (s1: char_star; s2: const_char_star; n: size_t): char_star;


(*   The strcat subroutine appends a copy of string s2 to the end
     of string s1.  The strncat subroutine copies at most n char-
     acters.  Both return a pointer to the null-terminated
     result. *)

<*EXTERNAL*> PROCEDURE strcat   (s1: char_star; s2: const_char_star): char_star;
<*EXTERNAL*> PROCEDURE strncat  (s1: char_star; s2: const_char_star; n: size_t): char_star;


(*   The strchr ( strrchr ) subroutine returns a pointer to the
     first (last) occurrence of character c in string s, or a
     NULL pointer if c does not occur in the string.  The null
     character terminating a string is considered to be part of
     the string.  *)

<*EXTERNAL*> PROCEDURE strchr   (s: const_char_star; c: int): char_star;
<*EXTERNAL*> PROCEDURE strrchr  (s: const_char_star; c: int): char_star;


(*   The strpbrk subroutine returns a pointer to the first
     occurrence in string s1 of any character from string s2, or
     a NULL pointer if no character from s2 exists in s1.  *)

<*EXTERNAL*> PROCEDURE strpbrk  (s1: const_char_star; s2: const_char_star): char_star;

(*   The strtok subroutine considers the string s1 to consist of
     a sequence of zero or more text tokens separated by spans of
     one or more characters from the separator string s2.  The
     first call (with pointer s1 specified) returns a pointer to
     the first character of the first token, and will have writ-
     ten a null character into s1 immediately following the
     returned token.  The function keeps track of its position in
     the string between separate calls, so that subsequent calls
     (which must be made with the first argument a NULL pointer)
     will work through the string s1 immediately following that
     token.  In this way, subsequent calls will work through the
     string s1 until no tokens remain.  The separator string s2
     may be different from call to call.  When no token remains
     in s1, a NULL pointer is returned.  *)

<*EXTERNAL*> PROCEDURE strtok   (s1: char_star; s2: const_char_star): char_star;


(*   The strcmp subroutine compares its arguments and returns an
     integer greater than, equal to, or less than 0, according as
     s1 is lexicographically greater than, equal to, or less than
     s2.  The strncmp subroutine makes the same comparison but
     looks at at most n characters.  *)

<*EXTERNAL*> PROCEDURE strcmp   (s1: const_char_star; s2: const_char_star): int;
<*EXTERNAL*> PROCEDURE strncmp  (s1: const_char_star; s2: const_char_star; n: size_t): int;


(*   The strlen subroutine returns the number of characters in s,
     not including the terminating null character.  *)

<*EXTERNAL*> PROCEDURE strlen   (s: const_char_star): size_t;


(*   The strspn ( strcspn ) subroutine returns the length of the
     initial segment of string s1 which consists entirely of
     characters from (not from) string s2.  *)

<*EXTERNAL*> PROCEDURE strspn   (s1: const_char_star; s2: const_char_star): int;
<*EXTERNAL*> PROCEDURE strcspn  (s1: const_char_star; s2: const_char_star): size_t;




(*

  (* Functions described in ANSI C but not available in Ultrix C *)

  <*EXTERNAL*> PROCEDURE memmove  (s1: void_star; s2: const_void_star; n: size_t): void_star;
  <*EXTERNAL*> PROCEDURE strcoll  (s1: const_char_star; s2: const_char_star): int;
  <*EXTERNAL*> PROCEDURE strxrfm  (s1: char_star; s2: const_char_star; n: size_t): size_t;
  <*EXTERNAL*> PROCEDURE strstr   (s1: const_char_star; s2: const_char_star): char_star;
  <*EXTERNAL*> PROCEDURE strerror (errnum: int): char_star;

*)

END Cstring.

