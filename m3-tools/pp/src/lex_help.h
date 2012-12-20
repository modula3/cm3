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

#if defined(__cplusplus) || __STDC__
#define USE_PROTOS
#endif

#ifdef __cplusplus
#define EXTERN_C extern "C"
#define EXTERN_C_BEGIN extern "C" {
#define EXTERN_C_END }
#else
#define EXTERN_C
#define EXTERN_C_BEGIN
#define EXTERN_C_END
#endif

int currentCol = 0; /* current column of input */
int currentRow = 0; /* current row of input */

#ifdef USE_PROTOS
void StopNPS (void);
void AddChar(char c);
#endif

#ifdef __cplusplus
#define input yyinput
#endif

#ifdef USE_PROTOS
void AddLexLength (void)
#else
AddLexLength ()
#endif
{
  size_t i;

  for (i = 0; i < yyleng; ++i)  AddChar(yytext[i]);
  if (yyleng >= lexbufsize) {
    fprintf (stderr, "Lex buffer overflow in pretty-printer\n");
    exit(-1); }
}

#ifdef USE_PROTOS
void AddChar(char c)
#else
AddChar(c)
    char c;
#endif
{
  ++lexposition;
  switch(c) {
    case '\n':
      currentCol = 0;
      currentRow++;
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

#ifdef USE_PROTOS
void BufferLexeme (int addLength)
#else
BufferLexeme (addLength)
int addLength;
#endif
{
    StopNPS();
    if (addLength) AddLexLength();
    lexptr = lexbufsize - lexptr;
    yylval = lexptr;
    strcpy (lexbuf + lexptr, yytext);
}

/* The routine CapBufferLexeme is like BufferLexeme, but it
 capitalizes as it copies. */

#ifdef USE_PROTOS
void CapBufferLexeme (int addLength)
#else
CapBufferLexeme (addLength)
int addLength;
#endif
{
    char *p, *q = yytext;
    StopNPS();

    if (addLength) AddLexLength();
    lexptr = lexbufsize - lexptr;
    yylval = lexptr;
    p = lexbuf + lexptr;
    while ((*p++ = toupper (*q++))) ;
}

/* The NPS information is saved in the comments array.  Elements 0 to
   nComments-1 refer to top-level comments or pragmas.  The NLs, startCol,
   and text fields are valid for them.  comments[nComments] contains only a
   newline count, giving the number of newlines after the final comment. */
struct Comment {
    int NLs;            /* number of newlines before this comment */
    int startCol;       /* start column of the comment */
    char *text;         /* text of the comment; NULL if none */
    long save;          /* temp */
};
struct Comment *comments = NULL;
int nComments = 0;
int nCommentsAlloced = 0;
static char *commText;      /* the text of the comments goes here */
static char *commTextPtr;   /* pointer to next available char */
static char *commTextLimit;

/* extern char *malloc(); */
/* extern char *realloc(); */

/* Make sure we have enough comment space allocated. */
#ifdef USE_PROTOS
static void AllocComments(int n)
#else
static AllocComments(n)
#endif
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

static char commentChar;    /* '(' or '<' */
static int commentLevel;    /* nesting level */

/* Save a char in the current comment (if any) and also count it for lex
   position and current column. */
#ifdef USE_PROTOS
static void SaveChar(char c)
#else
static SaveChar(c)
    char c;
#endif
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
#ifdef USE_PROTOS
static void StartComment(char c)
#else
static StartComment(c)
    char c;
#endif
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

#ifdef USE_PROTOS
static void EndComment(void)
#else
static EndComment()
#endif
{
   if (commentLevel == 1)
       SaveChar(0);     /* finish off the text. */
   --commentLevel;
}

#ifdef USE_PROTOS
static int IsWhite(char c)
#else
static int IsWhite(c)
    register char c;
#endif
{
    return c == ' ' || c == '\t' || c == '\f' || c == '\n' || c == '\r';
}

#ifndef __cplusplus
typedef enum {false, true} bool;
#endif

static bool inNPS = false;

#ifdef USE_PROTOS
void StartNPS (void)
#else
StartNPS ()
#endif
{
    if (inNPS) {
        return;
    }
    inNPS = true;
    commentLevel = 0;
    AllocComments(2);
    commTextPtr = commText;
    nComments = 0;
    comments[0].NLs = 0;
}

#ifdef USE_PROTOS
void StopNPS (void)
#else
StopNPS ()
#endif
{
    inNPS = false;
}

/* Handle white spaces.
   This was formerly part of HandleNPS. */
#ifdef USE_PROTOS
int HandleSpaces (void)
#else
int HandleSpaces ()
#endif
{
    StartNPS();
    /* Now deal with the main loop. */
    {
        int c = yytext[0];
        do {
            if (!IsWhite(c)) {
                unput(c);
                return WHITESPACE;
            }
            if (c == '\n') {
                ++comments[nComments].NLs;
            }
            SaveChar(c);
            c = input();
        } while (c > 0 /* EOF */);
    }
    return WHITESPACE;
}

/* Handle a "non-program-sequence."  This is a sequence of whitespace,
   comments and pragmas.  We only remember newlines, comments, and pragmas
   in the stuff we send to the parser.  The start column of the comments
   and pragmas are remembered in case the comment requests no formatting.

   When we arrive, the first character of whitespace or comment is in
   yytext, and we're responsible for taking care of the rest. */
#ifdef USE_PROTOS
int HandleCommentPragma (void)
#else
int HandleCommentPragma ()
#endif
{
    /* use 'int' instead of 'char' for distinguishing between end of file
       and characters above 127 */
    register int c, c2;

    StartNPS();
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
        /* Not in comment: this should never occur. */
        else if (commentLevel == 0) {
            fprintf(stderr, "outside a comment: bug in program\n");
        }
        /* In comment: check for comment end. */
        else {
            SaveChar(c);
            if (c == '*') {
                char target = commentChar == '(' ? ')' : '>';
                c2 = input();
                if (c2 == target) {
                    SaveChar(c2);
                    EndComment();
                    if (commentLevel == 0) {
                        return WHITESPACE;
                    }
                }
                else {
                    unput(c2);
                }
            }
        }
    } while ((c = input()) > 0 /* EOF */);
    return WHITESPACE;
}
