/* Copyright (C) 1989, Digital Equipment Corporation               */
/* All rights reserved.                                            */
/* See the file COPYRIGHT for a full description.                  */

/* Last modified on Wed Mar 13 12:28:39 PST 1996 by heydon         */
/*      modified on Thu Feb 23 14:08:31 PST 1995 by kalsow         */
/*      modified on Thu Apr 23 18:08:06 PDT 1992 by muller         */
/*      modified on Mon Apr 20 15:59:06 1992 by nichols@xerox.com  */
/*      modified on Mon Nov 25 17:41:09 PST 1991 by meehan         */


/* The routine  AddLexLength  is called by every lexical */
/* augment, in order to keep track of the number of */
/* characters that have been read.  It simply adds */
/* the length of  yytext  to the accumulated count in  lexposition, */
/* which is declared in pp.yacc . */
/* It also tracks the current column the input is in for comment */
/* processing. */

int currentCol = 0;	/* current column of input */

AddLexLength ()
{
  register int i;

  for (i = 0; i < yyleng; ++i)  AddChar(yytext[i]);
  if (yyleng >= lexbufsize) {
    fprintf (stderr, "Lex buffer overflow in pretty-printer\n");
    exit(-1); }
}

AddChar(c)
    char c;
{
  ++lexposition;
  switch(c) {
    case '\n':
      currentCol = 0;
      break;
    case '\t':
      /* Round up to next tab stop. */
      currentCol = (currentCol + 8) & ~7;
      break;
    default:
      ++currentCol;
      break;
  }
}

/* The routine BufferLexeme is used to copy a lexeme into */	
/* the buffer declared in pp.yacc (which see) where it can */
/* be accessed by the parser actions. */

BufferLexeme (addLength)
int addLength;
{ 
	if (addLength) AddLexLength();
	lexptr = lexbufsize - lexptr;
	yylval = lexptr;
	strcpy (lexbuf + lexptr, yytext);
}

/* The routine CapBufferLexeme is like BufferLexeme, but it
 capitalizes as it copies. */

CapBufferLexeme (addLength)
int addLength;
{ 
	char *p, *q = yytext;

	if (addLength) AddLexLength();
	lexptr = lexbufsize - lexptr;
	yylval = lexptr;
	p = lexbuf + lexptr;
	while (*p++ = toupper (*q++)) ;
}


/* The NPS information is saved in the comments array.  Elements 0 to
   nComments-1 refer to top-level comments or pragmas.  The NLs, startCol,
   and text fields are valid for them.  comments[nComments] contains only a
   newline count, giving the number of newlines after the final comment. */
struct Comment {
    int NLs;			/* number of newlines before this comment */
    int startCol;		/* start column of the comment */
    char *text;			/* text of the comment; NULL if none */
    long save;			/* temp */
};
struct Comment *comments = NULL;
int nComments = 0;
int nCommentsAlloced = 0;
static char *commText;		/* the text of the comments goes here */
static char *commTextPtr;	/* pointer to next available char */
static char *commTextLimit;

/* extern char *malloc(); */
/* extern char *realloc(); */

/* Make sure we have enough comment space allocated. */
static AllocComments(n)
{
    if (nCommentsAlloced == 0) {
	nCommentsAlloced = n+10;
	comments = (struct Comment *)
	    malloc(nCommentsAlloced * sizeof(*comments));
	/* Also allocate room for text. */
	commText = (char*) malloc(4096);
	commTextLimit = commText + 4096;
    }
    else if (nCommentsAlloced < n) {
	nCommentsAlloced = n+10;
	comments = (struct Comment *)
	    realloc(comments, nCommentsAlloced * sizeof(*comments));
    }
}

static char commentChar;	/* '(' or '<' */
static int commentLevel;	/* nesting level */

/* Save a char in the current comment (if any) and also count it for lex
   position and current column. */
static SaveChar(c)
    char c;
{
    if (c != 0)
	AddChar(c);
    if (commentLevel > 0) {
	if (commTextPtr >= commTextLimit) {
	    int i;
            long oldSize = commTextLimit - commText;
	    long newSize = oldSize + 4096;
	    /* Need to realloc the text.  We first save all the pointers as
	       offsets, do the realloc, then reset the pointers. */
	    for (i = 0; i < nComments; ++i)
		comments[i].save = comments[i].text - commText;
	    commText = (char*)realloc(commText, newSize);
	    commTextLimit = commText + newSize;
	    commTextPtr = commText + oldSize;
	    for (i = 0; i < nComments; ++i)
		comments[i].text = commText + comments[i].save;
	}
	*commTextPtr++ = c;
    }
}

/* Called when a comment is started, either at top-level or nested. */
static StartComment(c)
    char c;
{
    if (commentLevel == 0) {
	/* Starting a top-level comment.  Need to allocate two more than
	   what we have now: one for the new comment, and one for the extra
	   newline count we keep in comments[nComments]. */
	AllocComments(nComments+2);
	comments[nComments].startCol = currentCol;
	comments[nComments].text = commTextPtr;
	++nComments;
	comments[nComments].NLs = 0;
    }
    ++commentLevel;
    commentChar = c;
}

static EndComment()
{
   if (commentLevel == 1)
       SaveChar(0);		/* finish off the text. */
   --commentLevel;
}

static int IsWhite(c)
    register char c;
{
    return c == ' ' || c == '\t' || c == '\f' || c == '\n' || c == '\r';
}

/* Handle a "non-program-sequence."  This is a sequence of whitespace,
   comments and pragmas.  We only remember newlines, comments, and pragmas
   in the stuff we send to the parser.  The start column of the comments
   and pragmas are remembered in case the comment requests no formatting.

   When we arrive, the first character of whitespace or comment is in
   yytext, and we're responsible for taking care of the rest. */
HandleNPS ()
{
    register char c, c2;
    char target;
    int tok;
    char *p;
    /* Magic variable to tell us we found a special pragma last time. */
    static int pragmaToken = -1;

    if (pragmaToken != -1) {
	int tok = pragmaToken;
	pragmaToken = -1;
	input();		/* parse the '*' we pushed back. */
	/* Copy the comment into the normal token buffer. This assumes
	   lexbufsize > sizeof(yytext), but the code above assumes that,
	   anyway. */
	strcpy(yytext, comments[nComments].text);
	BufferLexeme(0);
	return tok;
    }
    commentLevel = 0;
    AllocComments(2);
    commTextPtr = commText;
    nComments = 0;
    comments[0].NLs = 0;
    /* Now deal with the main loop. */
    c = yytext[0];
    do {
	/* Check for a comment start whether we're in or out of a comment. */
	if ((commentLevel == 0 && (c == '(' || c == '<')) ||
	    (commentLevel > 0 && c == commentChar)) {
	    c2 = input();
	    if (c2 == '*') {
		StartComment(c);
		SaveChar(c);
		SaveChar(c2);
	    }
	    else if (commentLevel == 0) {
		unput(c2);
		unput(c);
		return WHITESPACE;
	    }
	    else {
		unput(c2);
		SaveChar(c);
	    }
	}
	/* Not in comment: check for newline or non-whitespace. */
	else if (commentLevel == 0) {
	    if (!IsWhite(c)) {
		unput(c);
		return WHITESPACE;
	    }
	    if (c == '\n')
		++comments[nComments].NLs;
	    SaveChar(c);
	}
	/* In comment: check for comment end. */
	else {
	    SaveChar(c);
	    if (c == '*') {
		c2 = input();
		target = commentChar == '(' ? ')' : '>';
		if (c2 == target) {
		    SaveChar(c2);
		    EndComment();
		    /* Check to see if this pragma is one of the ones we
		       return special tokens for.  We don't return them for
		       all pragmas since some can appear anywhere in a
		       program.  If this pragma matches, then return it (if
		       it's the first thing we saw) or push it back and
		       return whitespace.  Ugh.
		    
		    We also make sure the pragma isn't too large to fit in
		       our normal token buffer.  If it is, we give up and
		       treat it as a comment. */
		    if (commentLevel == 0 && (tok = CheckPragma()) != -1
			&& strlen(comments[0].text) < lexbufsize - 1) {
			/* Save it for next time. */
			--nComments;
			currentCol = comments[nComments].startCol;
			pragmaToken = tok;
			/* Now push back "<*" so the lexer will call us
			   next time. */
			unput('*');
			unput('<');
			return WHITESPACE;
		    }
		}
		else
		    unput(c2);
	    }
	}
    } while ((c = input()) > 0 /* EOF */);
    return WHITESPACE;
}

struct PragmaEntry {
    char *name;
    int len;
    int token;
};
static struct PragmaEntry pragmaTable[] = {
    "EXTERNAL", 8, PR_EXTERNAL,
    "INLINE", 6, PR_INLINE,
    "OBSOLETE", 8, PR_OBSOLETE,
    "UNUSED", 6, PR_UNUSED,
    NULL, 0, -1
};


/* See if the last pragma was one of our special ones. */
int 
CheckPragma()
{
    char *p = comments[nComments - 1].text;
    struct PragmaEntry *pe;
    int c;

    if (*p != '<')
	return -1;
    p += 2;
    while (IsWhite(*p))
	++p;
    for (pe = pragmaTable; pe->name != NULL; ++pe) {
	if (strncmp(p, pe->name, pe->len) == 0 &&
	    ((c = p[pe->len]) == '*' || IsWhite(c)))
	    return pe->token;
    }
    return -1;
}
