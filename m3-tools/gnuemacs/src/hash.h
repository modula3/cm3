/* Copyright (C) 1989, 1992, Digital Equipment Corporation                    */
/* All rights reserved.                                                       */
/* See the file COPYRIGHT for a full description.                             */

/* Last modified on Wed Aug 10 16:07:35 PDT 1994 by kalsow                    */
/*      modified on Fri Mar 20 20:10:44 PST 1992 by muller                    */
/*      modified on Fri Jan 31 16:09:58 PST 1992 by goldberg@xerox.com        */
/*      modified on Mon Jun  1 11:37:54 1987 by firefly                       */
/*      modified on  hania, Wed Jan  8 16:38:12 1986                          */

/* created by gnelson Wed Apr 16 20:39:35 1986                 */
/* File:  hash.h                                               */

/* Hash table method of recognizing keywords for lex.  The	*/
/* hash table is an array of pointers to objects of type	*/
/* KEWORDENTRY.  Each such object consists of two parts: the	*/
/* string representing the keyword (in capital letters), and	*/
/* the integer value of the lexeme corresponding to that	*/
/* keyword.  The objects corresponding to each of the keywords	*/
/* are stored in the array aok[].  There are NUMKEYWORDS of	*/
/* of them.							*/

/* The routine hash takes a string of characters, and returns	*/
/* a hash value for that string. 
	
/* The routine lookup takes a string and looks it up in the	*/
/* table, lookup returns a pointer to the hashtable entry	*/
/* which corresponds to its argument string, or NULL if there	*/
/* is no such entry.						*/
	
/* The routine install inserts a keyword into the hashtable.	*/
/* Its argument is a pointer to the KEYWORDENTRY object		*/
/* which describes the keyword.  Thus, inserting a keyword into */
/* the hashtable  consists of hashing it, finding an empty spot	*/
/* in the hashtable either directly at the hash index, or, in	*/
/* case of conflicts, at the first spot that's empty, and	*/
/* inserting install's second argument into that spot.		*/
	
/* The routine insert_keywords loops over all the entries in	*/
/* the array of keywords aok, and calls install on each of them.*/
	
/* This file is meant to be included in the yacc file.  It will	*/
/* not compile by itself, because it lacks the #define's for	*/
/* the names of the lexemes.					*/

#include <ctype.h>
	
#define NUMKEYWORDS (sizeof(aok)/sizeof(struct keywordEntry))
#define HASHSIZE 200
#ifndef NULL
#define NULL 0
#endif
#define TRUE 1
#define FALSE 0
	
typedef struct keywordEntry {
	char *keyword;
	int lexval;
} KEYWORDENTRY, *PTRKEYWORDENTRY;
	
static KEYWORDENTRY aok[] = {
	/* array of keywords */
"AND", AND,
"ANY", ANY,
"ARRAY", ARRAY,
"AS", AS,
"BEGIN", BGN,
"BITS", BITS,
"BRANDED", BRANDED,
"BY", BY,
"CASE", CASE,
"CONST", CONST,
"DIV", DIV,
"DO", DO,
"ELSE", ELSE,
"ELSIF", ELSIF,
"END", END,
"EVAL", EVAL,
"EXCEPT", EXCEPT,
"EXCEPTION", EXCEPTION,
"EXIT", EXIT,
"EXPORTS", EXPORTS,
"FINALLY", FINALLY,
"FOR", FOR,
"FROM", FROM,
"GENERIC", GENERIC,
"IF", IF,
"IMPORT", IMPORT,
"IN", IN,
"INTERFACE", INTERFACE,
"LOCK", LOCK,
"LOOP", LOOP,
"METHODS", METHODS,
"MOD", MOD,
"MODULE", MODULE,
"NOT", NOT,
"OBJECT", OBJECT,
"OF", OF,
"OR", OR,
"OVERRIDES", OVERRIDES,
"PROCEDURE", PROCEDURE,
"RAISE", RAISE,
"RAISES", RAISES,
"READONLY", READONLY,
"RECORD", RECORD,
"REF", REF,
"REPEAT", REPEAT,
"RETURN", RETURN,
"REVEAL", REVEAL,
"ROOT", ROOT,
"SET", SET,
"THEN", THEN,
"TO", TO, 
"TRY", TRY,
"TYPE", TYPE,
"TYPECASE", TYPECASE,
"UNSAFE", UNSAFE,
"UNTIL", UNTIL,
"UNTRACED", UNTRACED,
"VALUE", VALUE,
"VAR", VAR,
"WHILE", WHILE,
"WITH", WITH,
};
	
static PTRKEYWORDENTRY hashtab[HASHSIZE] = {
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};

int hash();
void install(), insertKeywords();
PTRKEYWORDENTRY lookup();

PTRKEYWORDENTRY *starthash = &(hashtab[0]);
PTRKEYWORDENTRY *endhash = &(hashtab[HASHSIZE]);
	
int
hash(s) char *s;
{
	int hashval;
	for (hashval=0; *s != '\0'; hashval += *s++);
	return(hashval % HASHSIZE);
}
	
PTRKEYWORDENTRY
lookup(s) char *s;
{
	int i;
	PTRKEYWORDENTRY *p, *p1;
	p = &hashtab[hash(s)];
	for (p1 = p; p1 < endhash && *p1 != NULL; p1++) {
		if (strcmp(s, (*p1)->keyword) == 0) return(*p1);
	}
	if (p1 == endhash){
		for(p1 = starthash; p1 < p && *p1 != NULL; p1++){
        	        if (strcmp(s, (*p1)->keyword) == 0) return(*p1);
		}
	}
	return(NULL);
}
  
void
install(where)
     PTRKEYWORDENTRY where;
{
	char *keyword;
	PTRKEYWORDENTRY *p1, *p;
	keyword = where->keyword;
	if (!lookup(keyword)){
		p = &(hashtab[hash(keyword)]);
		for (p1 = p; p1 < endhash && *p1 != NULL; p1++);
		if (p1 == endhash){
			for(p1 = starthash; p1 < p && *p1 != NULL; p1++);
			if (p1 == p) {
				printf("Keyword hashtable full\n");
				exit(-1);
			}
		}
		/* found an empty spot */
		*p1 = where;
	}
	else
		/*error */
		fprintf(stderr,
		  "lex: keyword %s already installed in hashtable\n", 
		  where->keyword);
}

void
insertKeywords()
{
	PTRKEYWORDENTRY p;
	for (p = aok; p < &aok[NUMKEYWORDS];p++)
		install(p);
}
