/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Thu Jun  2 15:41:03 PDT 1994 by kalsow     */
/*      modified on Wed Mar 25 11:24:20 1992 by hisgen         */

/* 
File: inttable.h
Integer hash table package, i.e., hash table where the keys
are integers.  
*/

#ifndef _inttable_h_
#define _inttable_h_

typedef char *_inttablepublic;
#define IntTable _inttablepublic

typedef char *IntTableDatum;

IntTable IntTableNew( /*nelems*/ );
  /* int nelems; 
  Creates a new, empty hash table.  The parameter nelems should be an
  estimate of hash table size.  Overflow is by external chaining, with 
  linear search of the external chain, so it is to the caller's advantage
  to make a good guess for nelems.*/

int IntTableGet( /*t, key, data*/ );
  /* IntTable t;
     int key;
     IntTableDatum *data;  -- by-ref parameter.
  Looks up key in the table t.  If found, returns true, with the
  ref-parameter data being assigned the corresponding data.
  If not found, returns false, with data undefined.*/

int IntTablePut( /*t, key, data*/ );
  /* IntTable t;
     int key;
     IntTableDatum data;
  Puts the pair (key, data) in the hash table t.  If an entry for
  key already existed, it is overwritten.  Returns true if there
  was already an entry with key, false otherwise.*/

int IntTableDelete( /*t, key, data*/ );
  /* IntTable t;
     int key;
     IntTableDatum *data;  -- by-ref parameter.
  If an entry for key exists in t, it is deleted, and true is
  returned, with the ref-parameter data being assigned the
  data item that (did) correspond to key.  If no entry for key
  exists in t, false is returned.*/

void IntTableFree( /*t*/ );
  /* IntTable t; 
  Frees the storage for the table t.  The "data" fields are
  *not* freed however.*/

#endif

