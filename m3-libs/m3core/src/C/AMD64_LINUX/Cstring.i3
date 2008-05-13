(* This file is the same or almost the same across all platforms, and
little used. Let's factor the commonality and eliminate the dead. *)

INTERFACE Cstring;

FROM Ctypes IMPORT const_char_star, void_star, const_void_star, int;
FROM Cstddef IMPORT size_t;

<*EXTERNAL*>
PROCEDURE strlen (s: const_char_star): size_t;

<*EXTERNAL*>
PROCEDURE memmove (s1: void_star; s2: const_void_star; n: size_t): void_star;

<*EXTERNAL*>
PROCEDURE memcmp (s1: const_void_star; s2: const_void_star; n: size_t): int;

<*EXTERNAL*>
PROCEDURE memcpy (s1: void_star; s2: const_void_star; n: size_t): void_star;

<*EXTERNAL*> 
PROCEDURE memset (s: void_star; c: int; n: size_t): void_star;

<*EXTERNAL*>
PROCEDURE strcmp (s1: const_char_star; s2: const_char_star): int;

<*EXTERNAL*>
PROCEDURE strncmp  (s1: const_char_star; s2: const_char_star; n: size_t): int;

END Cstring.
