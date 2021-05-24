(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Cstring;

FROM Ctypes IMPORT char_star, const_char_star, const_void_star, int, void_star;
FROM Cstddef IMPORT size_t;

<*EXTERNAL Cstring__memcpy *> PROCEDURE memcpy   (s1: void_star; s2: const_void_star; n: size_t): void_star;
<*EXTERNAL Cstring__memset *> PROCEDURE memset   (s: void_star; c: int; n: size_t): void_star;
<*EXTERNAL Cstring__memcmp *> PROCEDURE memcmp   (s1: const_void_star; s2: const_void_star; n: size_t): int;
<*EXTERNAL Cstring__strncpy*> PROCEDURE strncpy  (s1: char_star; s2: const_char_star; n: size_t): char_star;
<*EXTERNAL Cstring__strncat*> PROCEDURE strncat  (s1: char_star; s2: const_char_star; n: size_t): char_star;
<*EXTERNAL Cstring__strtok *> PROCEDURE strtok   (s1: char_star; s2: const_char_star): char_star;
<*EXTERNAL Cstring__strcmp *> PROCEDURE strcmp   (s1: const_char_star; s2: const_char_star): int;
<*EXTERNAL Cstring__strncmp*> PROCEDURE strncmp  (s1: const_char_star; s2: const_char_star; n: size_t): int;
<*EXTERNAL Cstring__strlen *> PROCEDURE strlen   (s: const_char_star): size_t;
<*EXTERNAL Cstring__strspn *> PROCEDURE strspn   (s1: const_char_star; s2: const_char_star): size_t;
<*EXTERNAL Cstring__strcspn*> PROCEDURE strcspn  (s1: const_char_star; s2: const_char_star): size_t;
<*EXTERNAL Cstring__memmove*> PROCEDURE memmove  (s1: void_star; s2: const_void_star; n: size_t): void_star;
<*EXTERNAL Cstring__strcoll*> PROCEDURE strcoll  (s1: const_char_star; s2: const_char_star): int;
<*EXTERNAL Cstring__strxfrm*> PROCEDURE strxfrm  (s1: char_star; s2: const_char_star; n: size_t): size_t;
<*EXTERNAL Cstring__strerror*> PROCEDURE strerror (errnum: int): char_star;

(* These are bad functions, that OpenBSD linker rightfully warns about, so
 * just provide the older style and then only folks using them will get warnings,
 * instead of everyone.
 *)
<*EXTERNAL *> PROCEDURE strcpy   (s1: char_star; s2: const_char_star): char_star;
<*EXTERNAL *> PROCEDURE strcat   (s1: char_star; s2: const_char_star): char_star;

END Cstring.
