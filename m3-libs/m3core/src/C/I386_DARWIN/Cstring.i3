(* Copyright according to COPYRIGHT-CMASS. *)
(* FIXME: copied from FreeBSD3 target. Probably needs to be changed. *)

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

