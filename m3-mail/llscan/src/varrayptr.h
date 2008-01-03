/* Copyright (C) 1994, Digital Equipment Corporation            */
/* All rights reserved.                                         */
/* See the file COPYRIGHT for a full description.               */
/*                                                              */
/* Last modified on Thu Jun  2 15:38:19 PDT 1994 by kalsow      */
/*      modified on Wed Mar 18 16:56:32 1992 by hisgen          */

/* 
File: varrayptr.h
Provides variable length array of pointers, VArrayPtr.
The array is dynamicly allocated, and expanded as necessary.  
The array indexing is zero-origin, i.e., element 0 is the
first element of the array.
Array elements are initialized to zero (i.e., NULL).
*/

#ifndef _varrayptr_h
#define _varrayptr_h

/*The fields of the VArrayPtr type are private to the
  implementation; the caller should not access them
  directly.*/
typedef struct _varrayptr {
  int highestIndex; 
  int numAllocated;
  char **a;
} VArrayPtr;

/*All the 'va' parameters are by-reference, i.e., "VArrayPtr *va"*/
void VAPtrInit(/* va, nelems */);  
char *VAPtrGet(/* va, index */);
void VAPtrSet(/* va, index, value */);
int VAPtrHighIndex(/* va */);  /*Returns the highest index that has ever
  been passed to VAPtrSet.*/
void VAPtrFree(/* va */);  

#endif
