/* Copyright (C) 2005, Purdue Research Foundation                  */
/* All rights reserved.                                            */
/* See the file COPYRIGHT-PURDUE for a full description.           */

#if __GNUC__ || __SUNPRO_C >= 0x590
#define ATTRIBUTE_NOINLINE __attribute__((noinline))
#else
#define ATTRIBUTE_NOINLINE
#endif

#if _MSC_VER >= 1300
#define DECLSPEC_NOINLINE __declspec(noinline)
#else
#define DECLSPEC_NOINLINE
#endif
