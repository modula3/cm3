/* 
Copyright 2002 Gary Bishop
Copyright 2002 Alan W. Irwin
This file is part of PLplot.

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as published by
the Free Software Foundation; version 2 of the License.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Library General Public License for more details.

You should have received a copy of the GNU Library General Public License
along with the file; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

/* 
A SWIG interface to PLplot for Modula3. This wrapper does the following:

   1) it strictly provides the C-API with the usual change of not
      requiring lengths for arrays,

   2) it attempts to provide the entire API *excluding* callbacks for
      plcont and plshade(s) (for now).
      
   3) it works both with the single and double-precision versions of the
      PLplot library.

This is known to work with swig-1.3.17 on Linux.

*/
%module plplotmodula3c
%include typemaps.i

%{
#include "plplotP.h"      
%}

#ifdef PL_DOUBLE
typedef double PLFLT;
#else
typedef float PLFLT;
#endif

typedef int PLINT;

/***************************
	A trick for docstrings
****************************/

%define DOC(func, string) 
%wrapper %{#define _doc_ ## func string %}
%enddef

/* Infrastructure for handling swig compatible plplot API definitions. */

#ifdef PL_DOUBLE
#define setup_array_1d_PLFLT setup_array_1d_d
#define setup_array_2d_PLFLT setup_array_2d_d
#define jPLFLTArray "C.double_star"
#define jPLFLTbracket "double[]"
#define jPLFLTbracket2 "double[][]"
#define GetPLFLTArrayElements GetDoubleArrayElements
#define ReleasePLFLTArrayElements ReleaseDoubleArrayElements
#define jPLFLT jdouble
#else
#define setup_array_1d_PLFLT setup_array_1d_f
#define setup_array_2d_PLFLT setup_array_2d_f
#define jPLFLTArray "C.float_star"
#define jPLFLTbracket "float[]"
#define jPLFLTbracket2 "float[][]"
#define GetPLFLTArrayElements GetFloatArrayElements
#define ReleasePLFLTArrayElements ReleaseFloatArrayElements
#define jPLFLT jfloat
#endif

%{
/*---------------------------------------------------------------------------
 * Array allocation & copy helper routines.  Note because of swig limitations
 * it is necessary to release the modula3 array memory right after calling these
 * routines.  Thus it is necessary to allocate and copy the arrays  even if 
 * the modula3 and plplot arrays are of the same type.  Note, because of this 
 * change to Geoffrey's original versions, caller must always free memory 
 * afterwards.  Thus, the must_free_buffers logic is gone as well.
 *---------------------------------------------------------------------------*/

/* 1d array of jints */

static void 
setup_array_1d_i( PLINT **pa, jint *adat, int n )
{
   int i;
   *pa = (PLINT *) malloc( n * sizeof(PLINT) );
   for( i=0; i < n; i++ ) {
      (*pa)[i] = adat[i];
   }
}

/* 1d array of jfloats */

static void 
setup_array_1d_f( PLFLT **pa, jfloat *adat, int n )
{
   int i;
   *pa = (PLFLT *) malloc( n * sizeof(PLFLT) );
   for( i=0; i < n; i++ ) {
      (*pa)[i] = adat[i];
   }
}

/* 1d array of jdoubles */

static void
setup_array_1d_d( PLFLT **pa, jdouble *adat, int n )
{
   int i;
   *pa = (PLFLT *) malloc( n * sizeof(PLFLT) );
   for( i=0; i < n; i++ ) {
      (*pa)[i] = adat[i];
   }
}

/* 2d array of floats */
/* Here caller must free(a[0]) and free(a) (in that order) afterward */

static void 
setup_array_2d_f( PLFLT ***pa, jfloat **adat, int nx, int ny )
{
   int i, j;

   *pa = (PLFLT **) malloc( nx * sizeof(PLFLT *) );
   (*pa)[0] = (PLFLT *) malloc( nx * ny * sizeof(PLFLT) );

   for( i=0; i < nx; i++ )
     {
	(*pa)[i] = (*pa)[0] + i*ny;
	for( j=0; j < ny; j++ )
	  (*pa)[i][j] = adat[i][j];
     }

}

/* 2d array of doubles */
/* Here caller must free(a[0]) and free(a) (in that order) afterward */

static void
setup_array_2d_d( PLFLT ***pa, jdouble **adat, int nx, int ny )
{
   int i, j;

   *pa = (PLFLT **) malloc( nx * sizeof(PLFLT *) );
   (*pa)[0] = (PLFLT *) malloc( nx * ny * sizeof(PLFLT) );

   for( i=0; i < nx; i++ )
     {
	(*pa)[i] = (*pa)[0] + i*ny;
	for( j=0; j < ny; j++ )
	  (*pa)[i][j] = adat[i][j];
     }

}
%}



/* I hate global variables but this is the best way I can think of to manage consistency
   checking among function arguments. */
%{
   static PLINT Alen = 0;
   static PLINT Xlen = 0, Ylen = 0;
   static PLFLT **xg;
   static PLFLT **yg;
  %}

/* The following typemaps take care of marshaling values into and out of PLplot functions. The
Array rules are trickly because of the need for length checking. These rules manage
some global variables (above) to handle consistency checking amoung parameters. 

Naming rules:
	Array 		(sets Alen to dim[0])
	ArrayCk 	(tests that dim[0] == Alen)
	ArrayX 		(sets Xlen to dim[0]
	ArrayCkX 	(tests dim[0] == Xlen)
	ArrayY 		(sets Ylen to dim[1])
	ArrayCkY 	(tests dim[1] == Ylen)
	Matrix 		(sets Xlen to dim[0], Ylen to dim[1])
	MatrixCk 	(test Xlen == dim[0] && Ylen == dim[1])
*/

/**********************************************************************************
			 PLINT arrays
**********************************************************************************/

/* with preceding count */
%typemap(in) (PLINT n, PLINT* Array) {
   jint *jxdata = (*jenv)->GetIntArrayElements( jenv, $input, 0 );
   $1 = (*jenv)->GetArrayLength( jenv, $input);
   Alen = $1;
   setup_array_1d_i( &$2, jxdata, Alen);
   /* Could find no easy way to do this as part of freearg so I modified
    * the previous function so it ALWAYS mallocs and copies so that
    * the modula3 array can be released immediately. */
   (*jenv)->ReleaseIntArrayElements( jenv, $input, jxdata, 0 );
}
%typemap(freearg) (PLINT n, PLINT* Array) {
   free($2);
}
%typemap(jni) (PLINT n, PLINT* Array) "jintArray"
%typemap(jtype) (PLINT n, PLINT* Array) "int[]"
%typemap(jstype) (PLINT n, PLINT* Array) "int[]"
%typemap(modula3in) (PLINT n, PLINT* Array) "$modula3input"
%typemap(modula3out) (PLINT n, PLINT* Array) {
   return $jnicall;
}
  
/* check consistency with previous */
%typemap(in) PLINT* ArrayCk {
   jint *jydata = (*jenv)->GetIntArrayElements( jenv, $input, 0 );
   if((*jenv)->GetArrayLength( jenv, $input ) != Alen) {
      printf("Vectors must be same length.\n");
      return;
   }
   setup_array_1d_i( &$1, jydata, Alen);
   (*jenv)->ReleaseIntArrayElements( jenv, $input, jydata, 0 );
}
%typemap(freearg) PLINT* ArrayCk {
   free($1);
}
%typemap(jni) PLINT* ArrayCk "jintArray"
%typemap(jtype) PLINT* ArrayCk "int[]"
%typemap(jstype) PLINT* ArrayCk "int[]"
%typemap(modula3in) PLINT* ArrayCk "$modula3input"
%typemap(modula3out) PLINT* ArrayCk {
   return $jnicall;
}
  
/* Weird case to allow argument to be one shorter than others */
%typemap(in) PLINT* ArrayCkMinus1 {
   jint *jydata = (*jenv)->GetIntArrayElements( jenv, $input, 0 );
   if((*jenv)->GetArrayLength( jenv, $input ) < Alen-1) {
      printf("Vector must be at least length of others minus 1.\n");
      return;
   }
   setup_array_1d_i( &$1, jydata, Alen);
   (*jenv)->ReleaseIntArrayElements( jenv, $input, jydata, 0 );
}
%typemap(freearg) PLINT* ArrayCkMinus1 {
   free($1);
}
%typemap(jni) PLINT* ArrayCkMinus1 "jintArray"
%typemap(jtype) PLINT* ArrayCkMinus1 "int[]"
%typemap(jstype) PLINT* ArrayCkMinus1 "int[]"
%typemap(modula3in) PLINT* ArrayCkMinus1 "$modula3input"
%typemap(modula3out) PLINT* ArrayCkMinus1 {
   return $jnicall;
}
  
/* No length but remember size to check others */
%typemap(in) PLINT *Array {
   jint *jydata = (*jenv)->GetIntArrayElements( jenv, $input, 0 );
   Alen = (*jenv)->GetArrayLength( jenv, $input);
   setup_array_1d_i( &$1, jydata, Alen);
   (*jenv)->ReleaseIntArrayElements( jenv, $input, jydata, 0 );
}
%typemap(freearg) PLINT *Array {
   free($1);
}
%typemap(jni) PLINT *Array "jintArray"
%typemap(jtype) PLINT *Array "int[]"
%typemap(jstype) PLINT *Array "int[]"
%typemap(modula3in) PLINT *Array "$modula3input"
%typemap(modula3out) PLINT *Array {
   return $jnicall;
}
  
/* Trailing count */
%typemap(in) (PLINT *ArrayCk, PLINT n) {
   jint *jydata = (*jenv)->GetIntArrayElements( jenv, $input, 0 );
   if((*jenv)->GetArrayLength( jenv, $input ) != Alen) {
      printf("Vectors must be same length.\n");
      return;
   }
   setup_array_1d_i( &$1, jydata, Alen);
   (*jenv)->ReleaseIntArrayElements( jenv, $input, jydata, 0 );
}
%typemap(freearg) (PLINT *ArrayCk, PLINT n) {
   free($1);
}
%typemap(jni) (PLINT *ArrayCk, PLINT n) "jintArray"
%typemap(jtype) (PLINT *ArrayCk, PLINT n) "int[]"
%typemap(jstype) (PLINT *ArrayCk, PLINT n) "int[]"
%typemap(modula3in) (PLINT *ArrayCk, PLINT n) "$modula3input"
%typemap(modula3out) (PLINT *ArrayCk, PLINT n) {
   return $jnicall;
}

/******************************************************************************
				 PLFLT Arrays 
******************************************************************************/

//temporary
#if 0
#ifndef PL_DOUBLE
%wrapper %{
/* some really twisted stuff to allow calling a single precision library from python */
PyArrayObject* myArray_ContiguousFromObject(PyObject* in, int type, int mindims, int maxdims)
{
  PyArrayObject* tmp = (PyArrayObject*)PyArray_ContiguousFromObject(in, PyArray_FLOAT,
								    mindims, maxdims);
  if (!tmp) {
    /* could be an incoming double array which can't be "safely" converted, do it anyway */
    if(PyArray_Check(in)) {
      PyErr_Clear();
      tmp = (PyArrayObject*)PyArray_Cast((PyArrayObject*)in, PyArray_FLOAT);
    }
  }
  return tmp;
}
 %}
#else
%wrapper %{
#define myArray_ContiguousFromObject PyArray_ContiguousFromObject
  %}
#endif

/* temporary*/
#endif
/* with preceding count */
%typemap(in) (PLINT n, PLFLT* Array) {
   jPLFLT *jxdata = (*jenv)->GetPLFLTArrayElements( jenv, $input, 0 );
   $1 = (*jenv)->GetArrayLength( jenv, $input);
   Alen = $1;
   setup_array_1d_PLFLT( &$2, jxdata, Alen );
   /* Could find no easy way to do this as part of freearg so I modified
    * the previous function so it ALWAYS mallocs and copies so that
    * the modula3 array can be released immediately. */
   (*jenv)->ReleasePLFLTArrayElements( jenv, $input, jxdata, 0 );
}
%typemap(freearg) (PLINT n, PLFLT* Array) {
   free($2);
}
%typemap(jni) (PLINT n, PLFLT* Array) jPLFLTArray
%typemap(jtype) (PLINT n, PLFLT* Array) jPLFLTbracket
%typemap(jstype) (PLINT n, PLFLT* Array) jPLFLTbracket
%typemap(modula3in) (PLINT n, PLFLT* Array) "$modula3input"
%typemap(modula3out) (PLINT n, PLFLT* Array) {
   return $jnicall;
}
  
/* check consistency with previous */
%typemap(in) PLFLT* ArrayCk {
   jPLFLT *jydata = (*jenv)->GetPLFLTArrayElements( jenv, $input, 0 );
   if((*jenv)->GetArrayLength( jenv, $input ) != Alen) {
      printf("Vectors must be same length.\n");
      return;
   }
   setup_array_1d_PLFLT( &$1, jydata, Alen );
   (*jenv)->ReleasePLFLTArrayElements( jenv, $input, jydata, 0 );
}
%typemap(freearg) PLFLT* ArrayCk {
   free($1);
}
%typemap(jni) PLFLT* ArrayCk jPLFLTArray
%typemap(jtype) PLFLT* ArrayCk jPLFLTbracket
%typemap(jstype) PLFLT* ArrayCk jPLFLTbracket
%typemap(modula3in) PLFLT* ArrayCk "$modula3input"
%typemap(modula3out) PLFLT* ArrayCk{
   return $jnicall;
}

/* set X length for later consistency checking */
%typemap(in) PLFLT *ArrayX {
   jPLFLT *jxdata = (*jenv)->GetPLFLTArrayElements( jenv, $input, 0 );
   Xlen = (*jenv)->GetArrayLength( jenv, $input);
   setup_array_1d_PLFLT( &$1, jxdata, Xlen);
   (*jenv)->ReleasePLFLTArrayElements( jenv, $input, jxdata, 0 );
}
%typemap(freearg) PLFLT *ArrayX {
   free($1);
}
%typemap(jni) PLFLT *ArrayX jPLFLTArray
%typemap(jtype) PLFLT *ArrayX jPLFLTbracket
%typemap(jstype) PLFLT *ArrayX jPLFLTbracket
%typemap(modula3in) PLFLT *ArrayX "$modula3input"
%typemap(modula3out) PLFLT *ArrayX {
   return $jnicall;
}

/* set Y length for later consistency checking */
%typemap(in) PLFLT *ArrayY {
   jPLFLT *jydata = (*jenv)->GetPLFLTArrayElements( jenv, $input, 0 );
   Ylen = (*jenv)->GetArrayLength( jenv, $input);
   setup_array_1d_PLFLT( &$1, jydata, Ylen);
   (*jenv)->ReleasePLFLTArrayElements( jenv, $input, jydata, 0 );
}
%typemap(freearg) PLFLT *ArrayY {
   free($1);
}
%typemap(jni) PLFLT *ArrayY jPLFLTArray
%typemap(jtype) PLFLT *ArrayY jPLFLTbracket
%typemap(jstype) PLFLT *ArrayY jPLFLTbracket
%typemap(modula3in) PLFLT *ArrayY "$modula3input"
%typemap(modula3out) PLFLT *ArrayY {
   return $jnicall;
}

/* with trailing count */
%typemap(in) (PLFLT *Array, PLINT n) {
   jPLFLT *jxdata = (*jenv)->GetPLFLTArrayElements( jenv, $input, 0 );
   $2 = (*jenv)->GetArrayLength( jenv, $input );
   setup_array_1d_PLFLT( &$1, jxdata, $2);
   (*jenv)->ReleasePLFLTArrayElements( jenv, $input, jxdata, 0 );
}
%typemap(freearg) (PLFLT *Array, PLINT n) {
   free($1);
}
%typemap(jni) (PLFLT *Array, PLINT n) jPLFLTArray
%typemap(jtype) (PLFLT *Array, PLINT n) jPLFLTbracket
%typemap(jstype) (PLFLT *Array, PLINT n) jPLFLTbracket
%typemap(modula3in) (PLFLT *Array, PLINT n) "$modula3input"
%typemap(modula3out) (PLFLT *Array, PLINT n) {
   return $jnicall;
}

/* check consistency with X dimension of previous */
%typemap(in) PLFLT* ArrayCkX {
   jPLFLT *jxdata = (*jenv)->GetPLFLTArrayElements( jenv, $input, 0 );
   if((*jenv)->GetArrayLength( jenv, $input ) != Xlen) {
      printf("Vectors must be same length.\n");
      return;
   }
   setup_array_1d_PLFLT( &$1, jxdata, Xlen );
   (*jenv)->ReleasePLFLTArrayElements( jenv, $input, jxdata, 0 );
}
%typemap(freearg) PLFLT* ArrayCkX {
   free($1);
}
%typemap(jni) PLFLT* ArrayCkX jPLFLTArray
%typemap(jtype) PLFLT* ArrayCkX jPLFLTbracket
%typemap(jstype) PLFLT* ArrayCkX jPLFLTbracket
%typemap(modula3in) PLFLT* ArrayCkX "$modula3input"
%typemap(modula3out) PLFLT* ArrayCkX{
   return $jnicall;
}

/* check consistency with Y dimension of previous */
%typemap(in) PLFLT* ArrayCkY {
   jPLFLT *jydata = (*jenv)->GetPLFLTArrayElements( jenv, $input, 0 );
   if((*jenv)->GetArrayLength( jenv, $input ) != Ylen) {
      printf("Vectors must be same length.\n");
      return;
   }
   setup_array_1d_PLFLT( &$1, jydata, Ylen );
   (*jenv)->ReleasePLFLTArrayElements( jenv, $input, jydata, 0 );
}
%typemap(freearg) PLFLT* ArrayCkY {
   free($1);
}
%typemap(jni) PLFLT* ArrayCkY jPLFLTArray
%typemap(jtype) PLFLT* ArrayCkY jPLFLTbracket
%typemap(jstype) PLFLT* ArrayCkY jPLFLTbracket
%typemap(modula3in) PLFLT* ArrayCkY "$modula3input"
%typemap(modula3out) PLFLT* ArrayCkY{
   return $jnicall;
}

/* 2D array with trailing dimensions, check consistency with previous */
%typemap(in) (PLFLT **MatrixCk, PLINT nx, PLINT ny) {
   jPLFLT **adat;
   jobject *ai;
   int nx = (*jenv)->GetArrayLength( jenv, $input );
   int ny = -1;
   int i, j;
   ai = (jobject *) malloc( nx * sizeof(jobject) );
   adat = (jPLFLT **) malloc( nx * sizeof(jPLFLT *) );

   for( i=0; i < nx; i++ )
     {
	ai[i] = (*jenv)->GetObjectArrayElement( jenv, $input, i );
	adat[i] = (*jenv)->GetPLFLTArrayElements( jenv, ai[i], 0 );

	if (ny == -1)
	  ny = (*jenv)->GetArrayLength( jenv, ai[i] );
	else if (ny != (*jenv)->GetArrayLength( jenv, ai[i] )) {
	   printf( "Misshapen a array.\n" );
	   for( j=0; j <= i; j++ )
	     (*jenv)->ReleasePLFLTArrayElements( jenv, ai[j], adat[j], 0 );
	   free(adat);
	   free(ai);
	   return;
	}
     }

   if( nx != Xlen || ny != Ylen ) {
      printf( "Vectors must match matrix.\n" );
      for( i=0; i < nx; i++ )
	(*jenv)->ReleasePLFLTArrayElements( jenv, ai[i], adat[i], 0 );
      free(adat);
      free(ai);
      return;
   }
   setup_array_2d_PLFLT( &$1, adat, nx, ny );
   $2 = nx;
   $3 = ny;
   for( i=0; i < nx; i++ )
     (*jenv)->ReleasePLFLTArrayElements( jenv, ai[i], adat[i], 0 );

   free(adat);
   free(ai);

}
%typemap(freearg) (PLFLT **MatrixCk, PLINT nx, PLINT ny) {
   free($1[0]);
   free($1);
}
%typemap(jni) (PLFLT **MatrixCk, PLINT nx, PLINT ny) "jobjectArray"
%typemap(jtype) (PLFLT **MatrixCk, PLINT nx, PLINT ny) jPLFLTbracket2
%typemap(jstype) (PLFLT **MatrixCk, PLINT nx, PLINT ny) jPLFLTbracket2
%typemap(modula3in) (PLFLT **MatrixCk, PLINT nx, PLINT ny) "$modula3input"
%typemap(modula3out) (PLFLT **MatrixCk, PLINT nx, PLINT ny) {
   return $jnicall;
}

/* 2D array with trailing dimensions, set the X, Y size for later checking */
%typemap(in) (PLFLT **Matrix, PLINT nx, PLINT ny) {
   jPLFLT **adat;
   jobject *ai;
   int nx = (*jenv)->GetArrayLength( jenv, $input );
   int ny = -1;
   int i, j;
   ai = (jobject *) malloc( nx * sizeof(jobject) );
   adat = (jPLFLT **) malloc( nx * sizeof(jPLFLT *) );

   for( i=0; i < nx; i++ )
     {
	ai[i] = (*jenv)->GetObjectArrayElement( jenv, $input, i );
	adat[i] = (*jenv)->GetPLFLTArrayElements( jenv, ai[i], 0 );

	if (ny == -1)
	  ny = (*jenv)->GetArrayLength( jenv, ai[i] );
	else if (ny != (*jenv)->GetArrayLength( jenv, ai[i] )) {
	   printf( "Misshapen a array.\n" );
	   for( j=0; j <= i; j++ )
	     (*jenv)->ReleasePLFLTArrayElements( jenv, ai[j], adat[j], 0 );
	   free(adat);
	   free(ai);
	   return;
	}
     }

   Xlen = nx;
   Ylen = ny;
   setup_array_2d_PLFLT( &$1, adat, nx, ny );
   $2 = nx;
   $3 = ny;
   for( i=0; i < nx; i++ )
     (*jenv)->ReleasePLFLTArrayElements( jenv, ai[i], adat[i], 0 );

   free(adat);
   free(ai);

}
%typemap(freearg) (PLFLT **Matrix, PLINT nx, PLINT ny) {
   free($1[0]);
   free($1);
}
%typemap(jni) (PLFLT **Matrix, PLINT nx, PLINT ny) "jobjectArray"
%typemap(jtype) (PLFLT **Matrix, PLINT nx, PLINT ny) jPLFLTbracket2
%typemap(jstype) (PLFLT **Matrix, PLINT nx, PLINT ny) jPLFLTbracket2
%typemap(modula3in) (PLFLT **Matrix, PLINT nx, PLINT ny) "$modula3input"
%typemap(modula3out) (PLFLT **Matrix, PLINT nx, PLINT ny) {
   return $jnicall;
}

/* 2D array, check for consistency */
%typemap(in) PLFLT **MatrixCk {
   jPLFLT **adat;
   jobject *ai;
   int nx = (*jenv)->GetArrayLength( jenv, $input );
   int ny = -1;
   int i, j;
   ai = (jobject *) malloc( nx * sizeof(jobject) );
   adat = (jPLFLT **) malloc( nx * sizeof(jPLFLT *) );

   for( i=0; i < nx; i++ )
     {
	ai[i] = (*jenv)->GetObjectArrayElement( jenv, $input, i );
	adat[i] = (*jenv)->GetPLFLTArrayElements( jenv, ai[i], 0 );

	if (ny == -1)
	  ny = (*jenv)->GetArrayLength( jenv, ai[i] );
	else if (ny != (*jenv)->GetArrayLength( jenv, ai[i] )) {
	   printf( "Misshapen a array.\n" );
	   for( j=0; j <= i; j++ )
	     (*jenv)->ReleasePLFLTArrayElements( jenv, ai[j], adat[j], 0 );
	   free(adat);
	   free(ai);
	   return;
	}
     }

   if( nx != Xlen || ny != Ylen ) {
      printf( "Vectors must match matrix.\n" );
      for( i=0; i < nx; i++ )
	(*jenv)->ReleasePLFLTArrayElements( jenv, ai[i], adat[i], 0 );
      free(adat);
      free(ai);
      return;
   }
   setup_array_2d_PLFLT( &$1, adat, nx, ny );
   for( i=0; i < nx; i++ )
     (*jenv)->ReleasePLFLTArrayElements( jenv, ai[i], adat[i], 0 );

   free(adat);
   free(ai);

}
%typemap(freearg) PLFLT **MatrixCk {
   free($1[0]);
   free($1);
}
%typemap(jni) PLFLT **MatrixCk "jobjectArray"
%typemap(jtype) PLFLT **MatrixCk jPLFLTbracket2
%typemap(jstype) PLFLT **MatrixCk jPLFLTbracket2
%typemap(modula3in) PLFLT **MatrixCk "$modula3input"
%typemap(modula3out) PLFLT **MatrixCk {
   return $jnicall;
}

%{
   typedef PLINT (*defined_func)(PLFLT, PLFLT);
   typedef void (*fill_func)(PLINT, PLFLT*, PLFLT*);
   typedef void (*pltr_func)(PLFLT, PLFLT, PLFLT *, PLFLT*, PLPointer);
   typedef PLFLT (*f2eval_func)(PLINT, PLINT, PLPointer);
   %}

/* First of two object arrays, where we check X and Y with previous.
 * Note this is the simplified Tcl-like approach to handling the xg
 * and yg arrays.  Later we would like to move to true call-back functions
 * here instead like is done with the python interface. */
%typemap(in) pltr_func pltr {
   jPLFLT **adat;
   jobject *ai;
   int nx = (*jenv)->GetArrayLength( jenv, $input );
   int ny = -1;
   int i, j;
   ai = (jobject *) malloc( nx * sizeof(jobject) );
   adat = (jPLFLT **) malloc( nx * sizeof(jPLFLT *) );

   for( i=0; i < nx; i++ )
     {
	ai[i] = (*jenv)->GetObjectArrayElement( jenv, $input, i );
	adat[i] = (*jenv)->GetPLFLTArrayElements( jenv, ai[i], 0 );

	if (ny == -1)
	  ny = (*jenv)->GetArrayLength( jenv, ai[i] );
	else if (ny != (*jenv)->GetArrayLength( jenv, ai[i] )) {
	   printf( "Misshapen a array.\n" );
	   for( j=0; j <= i; j++ )
	     (*jenv)->ReleasePLFLTArrayElements( jenv, ai[j], adat[j], 0 );
	   free(adat);
	   free(ai);
	   return;
	}
     }
   
   if( !((nx == Xlen && ny == Ylen) || (nx == Xlen && ny == 1))) {
      printf( "Xlen = %d, nx = %d, Ylen = %d, ny = %d\n", Xlen, nx, Ylen, ny );
      printf( "X vector or matrix must match matrix dimensions.\n" );
      for( i=0; i < nx; i++ )
	(*jenv)->ReleasePLFLTArrayElements( jenv, ai[i], adat[i], 0 );
      free(adat);
      free(ai);
      return;
   }
   /* Store whether second dimension is unity. */
   Alen = ny;
   setup_array_2d_PLFLT( &xg, adat, nx, ny );
   for( i=0; i < nx; i++ )
     (*jenv)->ReleasePLFLTArrayElements( jenv, ai[i], adat[i], 0 );

   free(adat);
   free(ai);
   $1 = pltr2;

}

%typemap(freearg) pltr_func pltr {
   free(xg[0]);
   free(xg);
}
%typemap(jni) pltr_func pltr "jobjectArray"
%typemap(jtype) pltr_func pltr jPLFLTbracket2
%typemap(jstype) pltr_func pltr jPLFLTbracket2
%typemap(modula3in) pltr_func pltr "$modula3input"
%typemap(modula3out) pltr_func pltr {
   return $jnicall;
}

/* Second of two object arrays, where we check X and Y with previous object. */
%typemap(in) PLPointer OBJECT_DATA {
   jPLFLT **adat;
   jobject *ai;
   int nx = (*jenv)->GetArrayLength( jenv, $input );
   int ny = -1;
   int i, j;
   PLcGrid2 cgrid;
   ai = (jobject *) malloc( nx * sizeof(jobject) );
   adat = (jPLFLT **) malloc( nx * sizeof(jPLFLT *) );

   for( i=0; i < nx; i++ )
     {
	ai[i] = (*jenv)->GetObjectArrayElement( jenv, $input, i );
	adat[i] = (*jenv)->GetPLFLTArrayElements( jenv, ai[i], 0 );

	if (ny == -1)
	  ny = (*jenv)->GetArrayLength( jenv, ai[i] );
	else if (ny != (*jenv)->GetArrayLength( jenv, ai[i] )) {
	   printf( "Misshapen a array.\n" );
	   for( j=0; j <= i; j++ )
	     (*jenv)->ReleasePLFLTArrayElements( jenv, ai[j], adat[j], 0 );
	   free(adat);
	   free(ai);
	   return;
	}
     }

   if( !((nx == Xlen && ny == Ylen) || (nx == Ylen && ny == 1 && ny == Alen))) {
      printf( "Xlen = %d, nx = %d, Ylen = %d, Alen = %d, ny = %d\n", 
	      Xlen, nx, Ylen, Alen, ny );
      printf( "Y vector or matrix must match matrix dimensions.\n" );
      for( i=0; i < nx; i++ )
	(*jenv)->ReleasePLFLTArrayElements( jenv, ai[i], adat[i], 0 );
      free(adat);
      free(ai);
      return;
   }
   setup_array_2d_PLFLT( &yg, adat, nx, ny );
   for( i=0; i < nx; i++ )
     (*jenv)->ReleasePLFLTArrayElements( jenv, ai[i], adat[i], 0 );

   free(adat);
   free(ai);
   cgrid.xg = xg;
   cgrid.yg = yg;
   cgrid.nx = nx;
   cgrid.ny = ny;
   $1 = &cgrid;

}

%typemap(freearg) PLPointer OBJECT_DATA {
   free(yg[0]);
   free(yg);
}
%typemap(jni) PLPointer OBJECT_DATA "jobjectArray"
%typemap(jtype) PLPointer OBJECT_DATA jPLFLTbracket2
%typemap(jstype) PLPointer OBJECT_DATA jPLFLTbracket2
%typemap(modula3in) PLPointer OBJECT_DATA "$modula3input"
%typemap(modula3out) PLPointer OBJECT_DATA {
   return $jnicall;
}

// Do not specify defined function or fill function from modula3.  Instead
// specify NULL and plfill defaults in the interface C code.
%typemap(in, numinputs=0) defined_func df {
     $1 = NULL;
}
%typemap(in, numinputs=0) fill_func ff {
     $1 = plfill;
}

/***************************
	String returning functions
****************************/

%typemap(jni) char *OUTPUT "jstring"
%typemap(jtype) char *OUTPUT "String"
%typemap(jstype) char *OUTPUT "String"
%typemap(modula3in) char *OUTPUT "$modula3input"
%typemap(modula3out) char *OUTPUT {
   return $jnicall;
}

/* The code below updates the jstring argument rather than returning a 
 * jstring because the fundamental function we are wrapping here has type
 * void, and that is wrapped by swig as a void JNI function, etc.  Anyhow, I
 * think the generated code is fine and should work for say, plgver, but it
 * doesn't.  (See commented out code in x01.modula3).
 * Probably, the best thing to do is to make a helper function for
 * each of plgdev, plgfnam, and plgver which returns a character string, and
 * then allow swig to wrap these helper functions, but I will leave that to
 * someone else.  Thus, for now, comment out all of this as well as
 * the plgdev, plgfnam, and plgver prototypes. */

// temporary
#if 0
/* This currently just used for plgdev, plgfnam, and plgver which apparently
 * have a limit of 80 bytes.  But to (hopefully) be safe for any future use
 * have a 1000 byte limit here. */
%typemap(in) char* OUTPUT (char buff[1000]){
  $1 = buff;
}
%typemap(argout) char *OUTPUT {
   $input = (*jenv)->NewStringUTF(jenv, buff$argnum);
}
%typemap(freearg) char *OUTPUT {}

// temporary
#endif
/* Character arrays: */

%typemap(jni) (PLINT *p_argc, char **argv) "jobjectArray"
%typemap(jtype) (PLINT *p_argc, char **argv) "String[]"
%typemap(jstype) (PLINT *p_argc, char **argv) "String[]"
%typemap(modula3in) (PLINT *p_argc, char **argv) "$modula3input"
%typemap(modula3out) (PLINT *p_argc, char **argv) {
   return $jnicall;
}
%typemap(in) (PLINT *p_argc, char **argv) (jint size) {
   int i = 0;
   size = (*jenv)->GetArrayLength(jenv, $input);
   $1 = &size;
   $2 = (char **) malloc((size+1)*sizeof(char *));
   /* make a copy of each string */
   for (i = 0; i<size; i++) {
      jstring j_string = (jstring)(*jenv)->GetObjectArrayElement(jenv, $input, i);
      const char * c_string = (*jenv)->GetStringUTFChars(jenv, j_string, 0);
/* Commented out version straight from swig documentation, but I think
 * it is wrong.
 *    $2[i] = malloc(strlen((c_string)+1)*sizeof(const char *)); */
      $2[i] = malloc((strlen(c_string)+1)*sizeof(const char *));
      strcpy($2[i], c_string);
      (*jenv)->ReleaseStringUTFChars(jenv, j_string, c_string);
      (*jenv)->DeleteLocalRef(jenv, j_string);
   }
   $2[i] = 0;
}

/* This cleans up the memory we malloc'd before the function call */
%typemap(freearg) (PLINT *p_argc, char **argv) {
   int i;
/* Commented out version straight from swig documentation, but I think
 * it is wrong.
 * for (i=0; i<size$argnum-1; i++) */
   for (i=0; i<size$argnum; i++)
     free($2[i]);
   free($2);
}

#if 0
%typemap(in) PLGraphicsIn *gin (PLGraphicsIn tmp) {
  if(!PySequence_Check($input) || PySequence_Size($input) != 2) {
    PyErr_SetString(PyExc_ValueError, "Expecting a sequence of 2 numbers.");
    return NULL;
  }
  $1 = &tmp;
  $1->dX = PyFloat_AsDouble(PySequence_Fast_GET_ITEM($input, 0));
  $1->dY = PyFloat_AsDouble(PySequence_Fast_GET_ITEM($input, 1));
}
%typemap(argout) PLGraphicsIn *gin {
  PyObject *o;
  o = PyFloat_FromDouble($1->wX);
  resultobj = t_output_helper(resultobj, o);
  o = PyFloat_FromDouble($1->wY);
  resultobj = t_output_helper(resultobj, o);
}
#endif

/* swig compatible PLplot API definitions from here on. */
%include plplotcapi.i
