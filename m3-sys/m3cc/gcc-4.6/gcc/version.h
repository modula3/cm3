/* Modula-3: modified */

#ifndef GCC_VERSION_H
#define GCC_VERSION_H

#ifdef __cplusplus
#define EXTERN_C_START extern "C" {
#define EXTERN_C_END }
#else
#define EXTERN_C_START
#define EXTERN_C_END
#endif

EXTERN_C_START

extern const char version_string[];
extern const char pkgversion_string[];
extern const char bug_report_url[];

EXTERN_C_END

#endif /* ! GCC_VERSION_H */
