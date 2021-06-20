/* Copyright (C) 1994, Digital Equipment Corporation    */
/* All rights reserved.                                 */
/* See the file COPYRIGHT for a full description.       */

/***************************************************************/
/* File: llscan.c                                              */
/* Last modified on Fri Feb 24 13:12:42 PST 1995 by kalsow     */
/*      modified on Thu May 12 13:18:04 PDT 1994 by msm        */
/*      modified on Tue Mar 23 17:23:15 1993 by hisgen         */
/*      modified on Fri Jun 29 16:54:18 PDT 1990 by swart      */
/*      modified on Sun Oct 15 21:52:21 PDT 1989 by roberts    */
/*      modified on Mon May 15  9:48:06 PDT 1989 by discolo    */
/* ----------------------------------------------------------- */
/*                                                             */
/* This file now has an edit history, at the end.  Please      */
/* keep it current.                                            */
/*                                                             */
/*      Program to construct a fast scan listing from a cache  */
/* which includes message inode numbers.                       */
/***************************************************************/

#if defined(__INTERIX) && !defined(_REENTRANT)
#define _REENTRANT
#endif

#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/file.h>
#include <signal.h>
#include <errno.h>

#include <SRCstdlib.h>
#include <strlib.h>
#include <file.h>

#include <varrayptr.h>
#include <inttable.h>

/***************************************************************/
/* Package constants                                           */
/***************************************************************/

#define CACHE ".inodecache"
#define DEFAULTNEWSDROP "/usr/spool/news"

#define DEFAULTMHBINDIR ""
  /*means to use default PATH*/

#define WIDTH 78
/*5-Aug-92: Increase MAXLINE from 100 to 1000: */
#define MAXLINE 1000
#define MAXARGLINE 5000
#define MHARGLIMIT 40

#define HASHLINE 80
typedef struct {
    ino_t inode;
    char header[HASHLINE];
} hashentryrec;
typedef hashentryrec *hashentryptr;


#ifndef DEFAULT_FORM
#define DEFAULT_FORM "/proj/generic/lib/scan.llscan"
#endif



/***************************************************************/
/* Package variables                                           */
/***************************************************************/

static struct stat statbuf;
static struct stat sinkstatbuf;
static char linkbuf[MAXPATHLEN+1];
static char tempbuf[MAXPATHLEN];
static char myhostname[MAXPATHLEN];
static char mhbindir[MAXPATHLEN];
static char incerrfile[MAXPATHLEN];
static string home;
static string mailDir;
static string foldername;
static string folder;
static string cachename;
static string headername;
static string llmovedname;
static string dotllscanname;
static bool madechanges = FALSE;
static string lastlinkname;
static string lastlinkvsname;

#define SCANCMDFUDGE 256
static char scancmd[MAXARGLINE];
static char *scanptr;
static int scanlen = 0;
static bool scancmdoverflow = FALSE;
static int scancount;
static int nmsgs;
static bool again = FALSE;

static char bboardname[MAXPATHLEN];
static char *bboardptr;
static string immutablebbname;
static int maxlink;
static int maxmsg;
static int minmsg;
static int curmsg;

static bool newmail;
static bool scanflag;
static bool cacheflag;
static bool scanuseform;
static bool userhasform = FALSE;

static bool appendflag = FALSE;
static bool bboard = FALSE;
static bool incflag = FALSE;
static bool islauralee = FALSE;  /*calling program really is lauralee(1)*/
static bool noexec = FALSE;
static bool purge = FALSE;
static bool quickpurge = FALSE;
static bool typeflag = FALSE;
static bool verbose = FALSE;
static bool warnings = TRUE;
static bool recursive = FALSE;

static bool didinc = FALSE;

static bool checkMhError = TRUE;
  /*Having this default to TRUE could generate alot of extra
    confusion for users.*/

static int exitstatus = 0;

static char linebuf[MAXLINE];

static VArrayPtr msgtable;
static IntTable hashtable;
/*     Inode numbers are cached to maintain track of message   */
/* numbers over folder operations (which preserve inode        */
/* numbers as long as standard rules are obeyed).              */




/***************************************************************/
/* Local function declarations                                 */
/***************************************************************/

static void InitHomeMailDir();
static void TooBigError();
static void ScanSwitch(/* cp */);
static void InitCache();
static void ReadCache();
static void DumpCache();
static void dorename(/* from, to */);
static void DumpCurMsg();
static void WriteIntToFile(/* fileName, i */);
static void mktempname(/* result, pattern */);
static void GetMyHostName();
static string sindex(/*subject, pattern*/);
static void ReadDirectory();
static void ReadScan();
static void InitScanArg();
static void AddScanArg(/* msgnum */);
static void AddIncAction();
static void UpdateBBoard();
static void DumpMaxLink();
static void SetMinMsg();
static void SetBBoardName();
static void AddBBLink(/* private, public */);
static int BBSelect(/* dp */);
static int BBCompare(/* dp1, dp2 */);
static string GetFolder(/* name */);
static bool FolderIsBBoard(/* name */);
static bool IsAMessage(/* name */);
static void warning(string msg, ...);
static const char *errno2str(/* err */);
static void ErrorHelp();

extern stream popen();



/***************************************************************/
/* Main program                                                */
/*                                                             */
/*     Sets the names of the various files and initializes     */
/* the scanning process.                                       */
/***************************************************************/
 
main(argc, argv)
int argc;
string argv[];
{
    signal(SIGPIPE, SIG_IGN);  /*Added 13-Sep-89 by hisgen.  SRC's
	distant process (dp) facility cleans up from some errors by
	killing the distant child with SIGPIPE.  But in the case
	of llscan, since there is (usually) no interesting output
	on stdout and stderr anyway, the best thing for llscan to do
	is to keep going.  Observe that llscan tends to be idempotent,
	so the human user can always run it again.*/
    signal(SIGHUP, SIG_IGN);  /*Because msh does clean up with SIGHUP*/

    { 
      string ce = getenv("llscan_check_mh_error");
      if (ce != NULL) {
	checkMhError = (*ce == 'T') || (*ce == 't') || (*ce == '1');
      }
    }

    while (argc > 1 && argv[1][0] == '-') {
	ScanSwitch(&argv[1][1]);
	argc--;
	argv++;
    }
    if (argc != 2) {
	fprintf(stderr, "Usage: llscan folder\n");
	exit(1);
    }
    InitHomeMailDir();
    { 
      int len;
      string mhd = getenv("MHBINDIR");
      if (mhd == NULL) mhd = DEFAULTMHBINDIR;
      strcpy(mhbindir, mhd);
      len = strlen(mhbindir);
      if ((len > 0) && (mhbindir[len-1] != '/')) mhbindir[len] = '/';
      if (verbose) { printf("mhbindir='%s'\n", mhbindir); fflush(stdout); }
      /*post: mhbindir is either the empty string or it ends with '/'*/
    }      
    if (! warnings) (void) freopen("/dev/null", "w", stderr);
    foldername = argv[1];
    if (foldername[0] == '+') foldername++;
    bboard = FolderIsBBoard(foldername);
    folder = GetFolder(foldername);
    if (folder == NULL) error("Folder '%s' does not exist", foldername);
    sprintf(tempbuf, "%s/%s", folder, CACHE);
    cachename = scopy(tempbuf);
    sprintf(tempbuf, "%s/+%s", folder, foldername);
    headername = scopy(tempbuf);
    sprintf(tempbuf, "%s/.llscan", folder);
    dotllscanname = scopy(tempbuf);
    lastlinkname = sconc(folder, "/.lastlink");
    lastlinkvsname = sconc(folder, "/.lastlinkvs");
    llmovedname = sconc(folder, "/.llmoved");

#ifdef vax
    /*assume ancient DEC SRC VAX mh*/
#define LARGESTLEGALMSG 999
    userhasform = FALSE;
    scanuseform = FALSE;
#else
#define LARGESTLEGALMSG 9999
    userhasform = FALSE;
    {
      stream profile;
      sprintf(tempbuf, "%s/.mh_profile", home);
      profile = fopen(tempbuf, "r");
      if (profile != NULL) {
        while (fgets(linebuf, MAXLINE, profile) != NULL) {
	  if ((strncmp(linebuf, "scan:", 5) == 0) &&
	      (sindex(linebuf, "-form") != NULL)) {
	    /*Matches either '-form' or '-format'*/
	    userhasform = TRUE;
	    break;
	  } /*if*/
	} /*while*/
        fclose(profile);
      } /*if*/
    }
    if (userhasform) scanuseform = FALSE;
    else 
      scanuseform = (stat(DEFAULT_FORM, &sinkstatbuf) != -1);
#endif
    if (verbose) {
      printf("userhasform=%d scanuseform=%d\n", userhasform, scanuseform);
      fflush(stdout);
    }      


    if (verbose) {
      printf("islauralee=%d '%s'\n", islauralee, dotllscanname);
      fflush(stdout);
    }
      
    InitCache();
    maxlink = -1;
    curmsg = minmsg = 0;
    if (bboard) SetBBoardName();
    if (appendflag) {
        if (!incflag || !streq(foldername, "inbox"))
	    error("Nonsensical switch combination");
	scanflag = FALSE;
	newmail = TRUE;
    } else {
	ReadCache();
	ReadDirectory();
	if (quickpurge) exit(exitstatus);
	newmail = (streq(foldername, "inbox") && incflag);
    }
    if (newmail) AddIncAction();
    if (noexec) {
	printf("%s%s\n", (scanflag) ? "" : "; ", scancmd);
        exit(exitstatus);
    }
    if (bboard) UpdateBBoard();
    if (scanflag && (nmsgs > 0 || newmail)) ReadScan();

    DumpCache();
    DumpCurMsg();
    if (bboard) DumpMaxLink();

    if (islauralee) { 
      unlink(dotllscanname); 
    } else {
      if (madechanges) {
        int fd = creat(dotllscanname, 0644);
        if (fd >= 0) close(fd);
      }
    }

    {
      /*Added 12-Nov-90 by hisgen: */
      struct timeval tv[2];
      static struct stat stCache;
      static struct stat stDir;
      int diff;
       
      if ((stat(head(cachename), &stDir) != -1) &&
          (stat(cachename, &stCache) != -1) &&
	  ((diff = (stDir.st_mtime - stCache.st_mtime)) > 0) &&
	  (diff < 5))  /*If not < 5, assume something wrong so do nothing.*/
      {
	  tv[0].tv_sec = stDir.st_mtime;
	  tv[0].tv_usec = 0;
          tv[1] = tv[0];
	  if (verbose) { printf("Calling utimes\n"); fflush(stdout); }
          if ((utimes(cachename, tv) < 0) && verbose) {
	      printf("Utimes failed\n");
	      fflush(stdout);
	  }
      } 
    }


    if (again && !appendflag && !noexec && !typeflag && !verbose) { 
      printf(
"Running llscan again, due to earlier near overflow of internal buffer.\n");
      fflush(stdout); 
      fflush(stderr);
      if (execlp("llscan", "llscan", foldername) < 0) {
        error("Could not execlp llscan - %M");
      }
    }


    if (exitstatus != 0) ErrorHelp();
    exit(exitstatus);
}



static void ErrorHelp()
{
    static bool did = FALSE;
    if (!did) {
      did = TRUE;
      fprintf(stderr, "%s%s%s",
	"llscan: We suggest trying your command again in a few minutes,\n",
	"  and if it fails repeatedly, get help from a mail wizard or\n",
	"  system administrator.\n");
    } /*if*/
} /*ErrorHelp*/




/***************************************************************/
/* ScanSwitch(str)                                             */
/*                                                             */
/*     Takes the letter sequence following a - and             */
/* interprets it as switches.                                  */
/***************************************************************/

static void ScanSwitch(cp)
char *cp;
{
    char c;

    while (c = *cp++) {
	switch (c) {
	    case 'a': appendflag = TRUE; break;
	    case 'b': /* obsolete */; break;
	    case 'i': incflag = TRUE; break;
	    case 'l': islauralee = TRUE; break;
	    case 'n': noexec = TRUE; break;
	    case 'p': purge = TRUE; break;
	    case 'q': quickpurge = TRUE; break;
	    case 'r': recursive = TRUE; break;
	    case 't': typeflag = TRUE; break;
	    case 'v': verbose = TRUE; break;
	    case 'w': warnings = FALSE; break;
	    default : error("Unrecognized option -%c", c);
	}
    }
}



/***************************************************************/
/* InitCache();                                                */
/*                                                             */
/*     Initializes the hashtable and msgtable structures       */
/* to be empty.                                                */
/***************************************************************/

static void InitCache()
{
    hashtable = IntTableNew(512);
    VAPtrInit(&msgtable, 32);
}



/***************************************************************/
/* CheckLLMoved();
/*
/*    Checks, heuristically, whether the folder directory has
/* been moved, e.g., to another file system, since we last ran
/* llscan.  If so, delete the .inodecache.
/* The heuristic is that the folder contains a new hidden file
/* ".llmoved".  The contents of this file are its own inode
/* number.  We stat the file and see if the number has changed.
/***************************************************************/

static void CheckLLMoved()
{
    static struct stat st;
    stream f;
    int fd;
    int llmver;
    ino_t ino;
#define LLMOVEDVER 1

    f = fopen(llmovedname, "r");
    if (f != NULL) {
      if ( (fscanf(f, "%d%u", &llmver, &ino) == 2) &&
           (llmver == LLMOVEDVER) &&
           (fstat(fileno(f), &st) >= 0) )
      {
        if (ino == st.st_ino) {
          if (verbose) { 
	    printf(".llmoved okay, no-op, ino=%u st_ino=%u\n", 
	      ino, st.st_ino); 
	    fflush(stdout); 
	  }
  	  fclose(f);
	  return;
	} else {
	  if (verbose) { 
	    printf("ino = %u; st.st_ino = %u\n", ino, st.st_ino);
	    printf(".llmoved moved, so deleting .inodecache\n"); 
	    fflush(stdout); 
	  }
	  madechanges = TRUE;
	  unlink(cachename);
	}
      }
      fclose(f);
    }

    if (verbose) { printf("Creating .llmoved\n"); fflush(stdout); }         
    f = fopen(llmovedname, "w");
    if (f != NULL) {
      if (fstat(fileno(f), &st) >= 0) {
        fprintf(f, "%d\n%u\n", LLMOVEDVER, st.st_ino);
      }
      fclose(f);
    }
} /*CheckLLMoved*/    

/***************************************************************/
/* ReadCache();                                                */
/*                                                             */
/*     Reads in the old cache file to determine a dictionary   */
/* of inode-to-message correspondences.                        */
/***************************************************************/

static void ReadCache()
{
    stream cachefile;
    char *cp;
    char *s;
    int inode;  
    int bucket;
    hashentryptr hp;

    CheckLLMoved();

    cachefile = fopen(cachename, "r");
    cacheflag = (cachefile != NULL);
    if (!cacheflag) {
      madechanges = TRUE;
      unlink(lastlinkname);  /*Added 26-Jun-92*/
      return;
    }
    while (fgets(linebuf, MAXLINE, cachefile) != NULL) {
        cp = (char*) index(linebuf, '\n');
        if (cp == NULL) {
	    warning("Scan line too long '%s'", linebuf);
	    continue;
	}
        *cp = '\0';
	inode = atoi(linebuf);
	hp = (hashentryptr) getmem(sizeof(hashentryrec));
	IntTablePut(hashtable, inode, hp);
	hp->inode = inode;
        s = (char*) index(linebuf, ':');  
	if (s == NULL) {
	    hp->header[0] = 0;
	} else {
	    s++;  /*skip over colon*/
  	    while (*s == ' ') s++;
	    while (isdigit(*s)) s++;
	    strncpy(hp->header, s, HASHLINE-1);
	    hp->header[HASHLINE-1] = 0;  
	} /*if*/
    } /*while*/
    fclose(cachefile);
}



#define DANG_NOT  0
#define DANG_PARTIAL  1
#define DANG_SURE  2

static int DanglingMsgName(name)
string name;
/*Assumes the following global variables have been initialized:
    bboard  folder  immutablebbname
*/
{
static char lbuf[MAXPATHLEN+1];
static struct stat mystbuf;  
int nch;

if (!bboard) return DANG_NOT;
if (stat(name, &sinkstatbuf) >= 0) return DANG_NOT;
if (errno != ENOENT) return DANG_NOT;

if (lstat(name, &mystbuf) < 0) return DANG_NOT;
if ((mystbuf.st_mode & S_IFMT) != S_IFLNK) return DANG_NOT;

nch = readlink(name, lbuf, MAXPATHLEN);
if (nch < 0) error("Cannot read symbolic link '%s' - %M", name);
if (nch == 0) return DANG_SURE;
lbuf[nch] = '\0';
if ( (stat(name, &sinkstatbuf) < 0) && (errno == ENOENT) &&
     (stat(immutablebbname, &sinkstatbuf) >= 0) &&
     (stat(name, &sinkstatbuf) < 0) && (errno == ENOENT) ) 
{
  if ( (lstat(lbuf, &sinkstatbuf) < 0) && (errno == ENOENT) ) return DANG_SURE;
  else return DANG_PARTIAL;
} else return DANG_NOT;
} /*DanglingMsgName*/


static int DanglingMsgNum(msgnum)
int msgnum;
{
static char name[MAXPATHLEN];
sprintf(name, "%s/%d", folder, msgnum);
return DanglingMsgName(name);
} /*DanglingMsgNum*/

static bool PurgeDangling(name)
string name;
{
if (verbose) { 
    printf("Purging dangling symbolic link '%s'\n", name);
    fflush(stdout);
}
if (unlink(name) < 0) {
    int uerr = errno;
    fprintf(stderr, 
        "Error: Could not delete dangling symbolic link '%s' - %s\n",
        name, errno2str(uerr));
    exitstatus = 1;
}
madechanges = TRUE;
} /*PurgeDangling*/

  


/***************************************************************/
/* DumpCache();                                                */
/*                                                             */
/*     DumpCache writes two files, the cached header file      */
/* specified by CACHE and the actual header file used by       */
/* mhe and lauralee.  The cache file is a duplicate of         */
/* the header file except that in place of the message         */
/* number, the cache file contains the inode number of         */
/* the message.                                                */
/***************************************************************/

static void DumpCache()
{
    int i;
    static char iQuaStr[20];
    stream cachefile, headerfile;
    hashentryptr hp;
    static char tempcachename[MAXPATHLEN];
    static char tempheadername[MAXPATHLEN];
    bool errc, errh;
    int errnoc, errnoh;
    int highindex;
    int iWidth;

    mktempname(tempcachename, cachename);
    mktempname(tempheadername, headername);

    errno = 0;
    if (appendflag) cachefile = fopen(cachename, "a");
    else cachefile = fopen(tempcachename, "w");
    if (cachefile == NULL) error("Can't open cache file '%s' - %M", cachename);

    errno = 0;
    if (appendflag) headerfile = fopen(headername, "a");
    else headerfile = fopen(tempheadername, "w");
    if (headerfile == NULL) error("Can't open header file '%s' - %M", 
      headername);

    sprintf(tempbuf, "%s/++", folder);
    (void) unlink(tempbuf);

    highindex = VAPtrHighIndex(&msgtable);
    for (i = 1; i <= highindex; i++) {
        hp = (hashentryptr) VAPtrGet(&msgtable, i);
	if (hp != NULL) {
	    if (hp->inode == 0 || hp->header[0] == 0) {
	        static struct stat stb;
		if (scancmdoverflow) continue;	        
	        sprintf(tempbuf, "%s/%d", folder, i);
		if (stat(tempbuf, &stb) < 0) {
		  int staterr = errno;

		  if (DanglingMsgName(tempbuf) == DANG_SURE) {
		    if (verbose || (exitstatus != 0)) {
		      printf("Purging dangling symbolic link '%s'\n", 
			tempbuf);
		      fflush(stdout);
		    }
		    (void) unlink(tempbuf);
		    madechanges = TRUE;
		    continue;
	          } /*if Dangling...*/

		  fprintf(stderr, 
"Error: Cannot access (using stat) message %d - %s\n", i, errno2str(staterr));
		  exitstatus = 1;
		} else if (stb.st_size == 0) {
		  warning("Message %d is a zero-length file", i);
		} else {
		  if (userhasform && i > LARGESTLEGALMSG) TooBigError();
		    /*Blame the fact that scan didn't produce a scan line
		      for this msg on the users -form*/
		  warning("Message %d has no recognizable header", i);
		}
		continue;
	    }
	    hp->header[0] = ' ';   
	    sprintf(iQuaStr, "%3d", i);
	    iWidth = strlen(iQuaStr);
	    fprintf(cachefile, "%06d:%3d%s\n", hp->inode, i, hp->header);
	    fprintf(headerfile, "%3d%.*s\n", i, WIDTH-iWidth, hp->header);
	    if (typeflag) {
		printf("%3d%.*s\n", i, WIDTH-iWidth, hp->header);
		fflush(stdout);
	    }
	}
    }
    errc = FALSE;
    errno = 0;
    fflush(cachefile);
    if (ferror(cachefile)) { errc = TRUE; errnoc = errno; }
    fclose(cachefile);
    errh = FALSE;
    errno = 0;
    fflush(headerfile);
    if (ferror(headerfile)) { errh = TRUE; errnoh = errno; }
    fclose(headerfile);
    if (errc) error("llscan: write - %s", errno2str(errnoc));
    if (errh) error("llscan: write - %s", errno2str(errnoh));

    if (! appendflag) {
        dorename(tempheadername, headername);
        dorename(tempcachename, cachename);
    }
}



/***************************************************************/
/* dorename(from, to)                                          */
/*                                                             */
/*     Does a rename(2), checking the return code: on error,   */
/* prints a msg and exits.				       */
/***************************************************************/

static void dorename(from, to)
string from, to;
{
    if (rename(from, to) < 0) {
        error("rename(2) of '%s' to '%s' failed - %M", from, to);
    }
}    



/***************************************************************/
/* DumpCurMsg();                                               */
/*                                                             */
/*     Dumps the current message number into the file cur      */
/* in the current folder.  If there should be no current       */
/* message indicator and this is not an incorporate action,    */
/* the cur file is removed.                                    */
/***************************************************************/

static void DumpCurMsg()
{
    sprintf(tempbuf, "%s/cur", folder);
    if (curmsg == 0) {
	if (!incflag) (void) unlink(tempbuf);
    } else {
	WriteIntToFile(tempbuf, curmsg);
    }
}



/***************************************************************/
/* WriteIntToFile(fileName, i)                                 */
/*                                                             */
/*     Writes the integer i to the file fileName, using        */
/* an intermediate temporary file and a rename(2) so as to     */
/* avoid problems with concurrent instances of llscan.         */
/***************************************************************/

static void WriteIntToFile(fileName, i)
string fileName;
int i;
{
    static char tempName[MAXPATHLEN];
    int fd;
    char buf[20];

    mktempname(tempName, fileName);
    fd = creat(tempName, 0755);
    if (fd < 0) error("llscan: creat of '%s' - %M", tempName);
    sprintf(buf, "%d\n", i);
    if (write(fd, buf, strlen(buf)) < 0) {
      error("llscan: write to file '%s' - %M", tempName);
    }
    close(fd);
    dorename(tempName, fileName);
}



/***************************************************************/
/* mktempname(result, pattern)                                 */
/*                                                             */
/*     Makes a temporary name in the string result.  The       */
/* string pattern is used to generate the temporary name by    */
/* the following method:  just before the last component of    */
/* the name, a ".," is inserted.  At the end of the name, the  */
/* current pid, the mktempname invocation count (which is      */
/* incremented each time we are called), and our hostname      */
/* are all appended, using the format "_%d_%d_%s".             */
/* The string pattern is not changed by this routine.          */
/* Unlike the Unix routine mktemp(3), XXXXXX's in the pattern  */
/* are NOT replaced by the current pid.                        */
/***************************************************************/

static void mktempname(result, pattern)
string result, pattern;
{
    string lastslash;
    int lastslashpos;
    static int callcnt = 0;
    char tempstr[50];

    callcnt++;
    lastslash = (char*) rindex(pattern, '/');
    if (lastslash != NULL) {
        lastslashpos = lastslash - pattern;
        strncpy(result, pattern, lastslashpos+1);
    } else {
        lastslashpos = -1;
    }
    result[lastslashpos+1] = '.';
    result[lastslashpos+2] = ',';
    result[lastslashpos+3] = 0;  /*NUL terminate*/
    strcat(result, &pattern[lastslashpos+1]);
    sprintf(tempstr, "_%d_%d_", getpid(), callcnt);
    strcat(result, tempstr);
    GetMyHostName();
    strcat(result, myhostname);
    if (verbose) { 
      printf("mktempname: '%s' -> '%s'\n", pattern, result);
      fflush(stdout);
    }
}

/***************************************************************/
/* GetMyHostName()                                             */
/*                                                             */
/*     Assigns our hostname to the global variable myhostname. */
/***************************************************************/

static void GetMyHostName()
{
    if (myhostname[0] != 0) return;  /*already done*/
    gethostname(myhostname, sizeof(myhostname)-1);  
}    



/* sindex(subject, pattern)
/*   Searches subject for the first occurrence of pattern.
/* If found, returns a pointer to this occurrence.  
/* If not found, null is returned.
*/

static string sindex(subject, pattern)
string subject, pattern;
{
if ((subject == NULL) || subject[0] == 0) return NULL;
if ((pattern == NULL) || pattern[0] == 0) return subject;
while (*subject) {
  if (*subject++ == *pattern) {
    char *stmp = subject;
    char *ptmp = pattern + 1;
    while ((*ptmp) && (*stmp++ == *ptmp++)) ;
    if (*ptmp == '\0') return(subject-1);
  } /*if*/
} /*while*/    
return NULL;
} /*sindex*/    



static void CheckTooBig(msgnum)
int msgnum;
{
    if (msgnum <= LARGESTLEGALMSG || scanuseform || userhasform) return;  
	/*Okay.  In particular, if we're scaning with the -form flag, then
	we can handle message numbers wider than 4 digits.  However, if we
	didn't supply our own -form, the builtin form within MH (even the
	new MH) messes up on wider message numbers.  We assume that
	the user's -form is also adequate -- downstream we will give
	an error message if scan doesn't produce the scan line.
	--Andy Hisgen, 3-Feb-93.*/
    TooBigError();
} /*CheckTooBig*/    

static void TooBigError()
{
    if (!recursive && bboard && !appendflag && !noexec && !typeflag) { 
	fflush(stdout);
	if (verbose) {
	  fprintf(stderr, 
"Warning: calling llscan recursively to remove dangling symbolic links.\n");
	  fflush(stderr);
        }	  
        system(sconc(sconc("llscan -pr ", verbose ? "-v " : ""), foldername));
    } /*if*/

    fprintf(stderr, 
"Error: folder +%s contains messages bigger than %d.\n", 
      foldername, LARGESTLEGALMSG);
    fprintf(stderr, 
"First, try packing it.\n");

    if (bboard) {
      fprintf(stderr, 
"If that doesn't help, delete some messages -- if you're using Postcard,\n");
      fprintf(stderr,
"try the Purge command on the Folders menu.\n");
    } else {
      fprintf(stderr,
"If that doesn't help, you will have to move some messages to another\n");
      fprintf(stderr,
"folder or delete them.\n");
    } /*if*/

    if (userhasform) {
      fprintf(stderr, 
"Also, your .mh_profile is telling the mh scan(1) program to use a -form\n");
      fprintf(stderr,
"and perhaps the form you're supplying cannot handle messages bigger than\n");
      fprintf(stderr,
"%d.  Look to see if the -form file contains the string '4(msg)'.\n",
        LARGESTLEGALMSG);
    } /*if*/
    exit(1);
} /*TooBigError*/



/***************************************************************/
/* ReadDirectory();                                            */
/*                                                             */
/*     In preparing the header file, "truth" is represented    */
/* by the contents of the folder directory.  This function     */
/* iterates over the directory, looking up the inode values    */
/* of each message in the cached table of headers.  Any        */
/* messages which are not found in the table are added         */
/* to a list of items to be regenerated in ReadScan.           */
/***************************************************************/

static void ReadDirectory()
{
    DIR *dirfile;
    struct direct *dp;
    int msgnum;
    hashentryptr hp;
    int highindex;
    int minfoldermsg;
    int checkdanglecnt, stopdanglecnt;
    bool cheappurge;
    int ino;
    int mcnt, mcntHalf, mcntFourth;

    scanflag = !cacheflag;
    InitScanArg();
    nmsgs = 0;
    maxmsg = 0;
    cheappurge = FALSE;
    minfoldermsg = 2000000000;  /*Bigger than any actual msg number*/

    dirfile = opendir(folder);
    if (dirfile == NULL) error("Can't open folder '%s' - %M", folder);
    mcnt = 0;
    while (1) {
        errno = 0;
	dp = readdir(dirfile);
	if (dp == NULL) {
	  if (errno == 0) break;
	  else error("Reading directory '%s' - %M", folder);
	}
	if (!IsAMessage(dp->d_name)) continue;
	msgnum = atoi(dp->d_name);
	VAPtrSet(&msgtable, msgnum, dp->d_ino);
	if ((msgnum > 0) && (msgnum < minfoldermsg)) minfoldermsg = msgnum;
	mcnt++;
    } /*while*/
    closedir(dirfile);
    mcntHalf = mcnt>>1;
    mcntFourth = mcntHalf>>1;

#define NDANGLE 5
    checkdanglecnt = 0;
    stopdanglecnt = 0;
    mcnt = 0;
    highindex = VAPtrHighIndex(&msgtable);
    for (msgnum = minfoldermsg; msgnum <= highindex; msgnum++) {
      if ((ino = (int) VAPtrGet(&msgtable, msgnum)) != 0) {
        if (bboard) {
	  mcnt++;
	  if (purge || cheappurge || (!cacheflag)) {
	      sprintf(tempbuf, "%s/%d", folder, msgnum);
	      if (DanglingMsgName(tempbuf) == DANG_SURE) {
		  PurgeDangling(tempbuf);
		  VAPtrSet(&msgtable, msgnum, NULL);
		  continue;
	      } else if (cheappurge && (stopdanglecnt++ > NDANGLE)) {
		  cheappurge = FALSE;
		  if (verbose) {
		    printf("cheappurge=FALSE since '%s' exists\n", tempbuf);
		    fflush(stdout);
		  }
	      } /*if-else dangling*/
	  } else if (checkdanglecnt <= NDANGLE 
	  	     /* 23Mar93 not yet || mcnt == mcntFourth || mcnt == mcntHalf */ ) 
	  {
	      checkdanglecnt++;
	      sprintf(tempbuf, "%s/%d", folder, msgnum);
	      if (DanglingMsgName(tempbuf) == DANG_SURE) {
	          PurgeDangling(tempbuf);
		  VAPtrSet(&msgtable, msgnum, NULL);
		  cheappurge = TRUE;
	          if (verbose) { 
		      printf("cheappurge=TRUE\n"); fflush(stdout); 
		  }
		  continue;
	      } /*if*/
	  } /*if-else purge...*/
	} /*if bboard...*/

        ++nmsgs;
	if (msgnum > maxmsg) maxmsg = msgnum;
        if (IntTableGet(hashtable, ino, &hp)) {
	    /*Already in table*/
            VAPtrSet(&msgtable, msgnum, hp);	
	} else {
	    /*Not yet in table*/
            int dang;
	    sprintf(tempbuf, "%s/%d", folder, msgnum);
	    dang = DanglingMsgName(tempbuf);
	    if (dang == DANG_NOT) {
	      hp = (hashentryptr) getmem(sizeof(hashentryrec));
	      IntTablePut(hashtable, ino, hp);
	      hp->inode = ino;	    
	      hp->header[0] = 0;
	      if (cacheflag) AddScanArg(msgnum);
              VAPtrSet(&msgtable, msgnum, hp);	
	    } else {
	      VAPtrSet(&msgtable, msgnum, NULL);
	      if (dang == DANG_SURE) {
		if (verbose) {
		  printf("Purging dangling symbolic link '%s'\n", tempbuf);
		  fflush(stdout);
		}
		if (unlink(tempbuf) < 0) {
		    int uerr = errno;
		    fprintf(stderr, 
"Error: Could not delete dangling symbolic link '%s' - %s\n",
		      tempbuf, errno2str(uerr));
		    exitstatus = 1;
		}
		madechanges = TRUE;
	      } else {
		if (verbose) {
		  printf("Ignoring partial dangling symbolic link '%s'\n", 
		    tempbuf);
		  fflush(stdout);
		}
	      }
	    }      
        }
      } /*if*/
    } /*for*/
    CheckTooBig(highindex);
}



/***************************************************************/
/* ReadScan();                                                 */
/*                                                             */
/*     Reads information on new messages by regenerating       */
/* the headers using the mh scan program.  The messages        */
/* to be read are contained in the command string (scancmd)    */
/* which has been prepared in the directory scan.              */
/*                                                             */
/*     If the sentinel "<>" appears, then subsequent           */
/* messages are being added from an inc scan.  This            */
/* means that these must be added explicitly to the            */
/* msgtable.                                                   */
/***************************************************************/

static void ReadScan()
{
    stream scanpipe;
    char *cp;
    int msgnum;
    hashentryptr hp;
    int inccount;
    int childstatus;
    char *s;

    if (verbose) { printf("scancmd='%s'\n", scancmd); fflush(stdout); }
    scanpipe = popen(scancmd, "r");
    inccount = -1;
    if (scanpipe == NULL) error("Can't exec scan");
    while (fgets(linebuf, MAXLINE, scanpipe) != NULL) {
        cp = (char*) index(linebuf, '\n');
        if (cp == NULL) {
	    warning("Scan line too long '%s'", linebuf);
	    continue;
	}
	if (cp - linebuf > WIDTH) cp = linebuf + WIDTH;
        *cp = '\0';
        if (verbose) { printf("%s\n", linebuf); fflush(stdout); }
        if (streq(linebuf, "<>")) {
	    inccount = 0;
	    continue;
        }
	msgnum = atoi(linebuf);
	if (msgnum == 0) continue;
	sprintf(tempbuf, "%s/%d", folder, msgnum);
	if (lstat(tempbuf, &statbuf) < 0) {
	    warning("Message %d has disappeared", msgnum);
	    continue;
	}
	if (inccount >= 0) {
	    ++inccount;
	    didinc = TRUE;
	    hp = (hashentryptr) getmem(sizeof(hashentryrec));
	    IntTablePut(hashtable, (int) statbuf.st_ino, hp);
	    VAPtrSet(&msgtable, msgnum, hp);
        } else {
	    hp = (hashentryptr) VAPtrGet(&msgtable, msgnum);
	    if (hp == NULL) {
		warning(
"Message %d was produced by scan but was not requested", msgnum);
		continue;
	    }		
        }
	madechanges = TRUE;
	hp->inode = statbuf.st_ino;
	s = linebuf;
	while (*s == ' ') s++;
	while (isdigit(*s)) s++;
	strncpy(hp->header, s, HASHLINE-1);
	hp->header[HASHLINE-1] = 0;
    }
    /*Added checking of childstatus 12-Nov-90 by hisgen.  Before,
      it just used fclose: */
    fflush(stderr);
    fflush(stdout);
    childstatus = pclose(scanpipe);
    if ((childstatus != 0) && checkMhError) {
      /*See if inc(1) error was due to "no mail to incorporate".
        If so, suppress the error message and error return code.*/
      if (incerrfile[0] != 0) {
        int nonempty = 0;
        stream ef = fopen(incerrfile, "r");
	if (ef != NULL) {
	  while(fgets(linebuf, MAXLINE, ef) != NULL) {
	    nonempty++;
	    { 
	      char *s;
	      s = linebuf;
	      while (*s != 0) { *s = tolower(*s); s++; }
            }
	    if(sindex(linebuf, "no mail to incorporate") != NULL) { 
	      fclose(ef); 
	      unlink(incerrfile);
	      return; 
	    } /*if*/
	  } /*while*/
	  if (nonempty) {
	    rewind(ef);
	    fprintf(stderr, "inc: error(s):\n");
	    fflush(stderr);
	    while(fgets(linebuf, MAXLINE, ef) != NULL) {
	      fputs(linebuf, stderr);
	    } /*while*/
	  } /*if nonempty*/
	  fclose(ef);
	} /*if open succeeded*/
      } /*if incerrfile*/
      exitstatus = 1;
      fprintf(stderr, 
        "llscan: call of mh scan or inc program exited non-zero 0x%x\n",
        childstatus);
    } 
    if (incerrfile[0] != 0) unlink(incerrfile);
}



/***************************************************************/
/* InitScanArg();                                              */
/*                                                             */
/*     Initializes the scan argument data for subsequent       */
/* calls to AddScanArg and AddIncAction.  The cumulative       */
/* effect of these routines is to produce a string in          */
/* scancmd which can be passed to sh in the popen call.        */
/***************************************************************/

static void ScanCheckOverflow(k)
int k;
{
    if (scancmdoverflow) return;
    if (scanlen + k >= (sizeof(scancmd) - SCANCMDFUDGE)) {
      scancmdoverflow = TRUE;
      fprintf(stderr, "%s%s",
"Error: internal shell command line buffer would overflow, so will not\n",
"  scan all messages.  Suggested fix: just try running llscan again.\n");
      fflush(stderr);
      again = TRUE;      
    }
} /*ScanCheckOverflow*/    

static void ScanCat(s)
char *s;
{
    int slen = strlen(s);
    ScanCheckOverflow(slen);
    if (scancmdoverflow) return;
    strcpy(scanptr, s);
    scanptr += slen;
    scanlen += slen;
} /*ScanCat*/    
      

static void AddForm()
{
    if (scanuseform) {
        ScanCat("-form ");
        ScanCat(DEFAULT_FORM);
        ScanCat(" ");
    }
}

static void InitScanArg()
{
    scanptr = scancmd;
    scanlen = 0;
    scancmdoverflow = FALSE;
    scancount = 0;
    ScanCat(mhbindir);
    ScanCat("scan ");
    AddForm();
    ScanCat("+");
    ScanCat(foldername);
    if (verbose) { printf("scancmd='%s'\n", scancmd); fflush(stdout); }
}



/***************************************************************/
/* AddScanArg(msgnum);                                         */
/*                                                             */
/*     Adds the indicated message to the scancmd argument      */
/* list.  The scancount arg is required to avoid the bug in    */
/* the mh commands that does not allow more than MHARGLIMIT    */
/* arguments to be specified in a single command.              */
/***************************************************************/

static void AddScanArg(msgnum)
int msgnum;
{
    static char msgstr[20];

    CheckTooBig(msgnum);
    scanflag = TRUE;
    sprintf(msgstr, " %d", msgnum);
    if (++scancount > MHARGLIMIT) {
        ScanCat(" ; ");
        ScanCheckOverflow(1024+5+33+1+strlen(foldername)+strlen(msgstr));
	ScanCat(mhbindir);
	ScanCat("scan ");	
        AddForm();
	ScanCat("+");
	ScanCat(foldername);
        if (verbose) { printf("scancmd='%s'\n", scancmd); fflush(stdout); }
	scancount = 0;
    }
    ScanCat(msgstr);
}



/***************************************************************/
/* AddIncAction()                                              */
/*                                                             */
/*     When the folder is the inbox and mail exists, the       */
/* program automatically calls inc during the scan             */
/* operation to incorporate any new mail.  Since the           */
/* inc operation returns the scan headers anyway, this         */
/* can be parsed together.  Since this does not, however,      */
/* show up in the directory scan, the reader must note         */
/* this and add these messages directly to msgtable.           */
/* This is noted by including the sentinel "<>" between        */
/* the scan data and the inc action.                           */
/***************************************************************/

static void AddIncAction()
{
    if (!scanflag) {
	scanflag = TRUE;
        scanptr = scancmd;
	scanlen = 0;
    } else {
	ScanCat(" ; ");
    }
    ScanCat("echo '<>' ; ");
    mktempname(incerrfile, "/tmp/llscanincerr");
    ScanCheckOverflow(1024+4+33+6+3+strlen(incerrfile));
    ScanCat(mhbindir);
    ScanCat("inc ");
    AddForm();
    ScanCat("+inbox");
    ScanCat(" 2>");
    ScanCat(incerrfile);
    if (verbose) { printf("scancmd='%s'\n", scancmd); fflush(stdout); }
}



/***************************************************************/
/* UpdateBBoard()                                              */
/*                                                             */
/*     For bulletin boards, we read through the associated     */
/* real folder scanning for message numbers higher than the    */
/* one referred to by the largest message in the folder.       */
/* These are linked back to consecutively increasing message   */
/* numbers in the private folder.  This would require a        */
/* sort if we used opendir, so scandir is used.                */
/***************************************************************/

static void UpdateBBoard()
{
    int i, n;
    struct direct **namelist;

    SetMinMsg();
    n = scandir(bboardname, &namelist, BBSelect, BBCompare);
    if (n < 0) error("No spool directory for bboard '%s' - %M", bboardname);
    if (n > 0) {
        curmsg = maxmsg + 1;
        for (i = 0; i < n; i++) {
	    maxmsg++;
            AddBBLink(maxmsg, maxlink = atoi(namelist[i]->d_name));
	} /*for*/
    }
    free ((char *) namelist);
}



/***************************************************************/
/* DumpMaxLink();                                              */
/*                                                             */
/*     As a convenience for lauralee, bulletin board folders   */
/* also contain a file .lastlink which contains the image of   */
/* the last message in the last scan.                          */
/*     However, (as of 26-Jun-92), if the target file that     */
/* would be named by the new .lastlink file doesn't exist, we   */
/* instead delete the .lastlink file.                          */
/***************************************************************/

static void DumpMaxLink()
{
    static struct stat st;
    stream vsfile;
    int statflag, staterrno;
    int newval;

    newval = (maxlink == -1) ? minmsg : maxlink;
    if (verbose) {
      printf("DumpMaxlink:  maxlink=%d  minmsg=%d  newval=%d\n",
	maxlink, minmsg, newval);
      fflush(stdout);
    }
    sprintf(tempbuf, "%s/%d", immutablebbname, newval);
    statflag = stat(tempbuf, &st);
    staterrno = errno;
    if ( (newval != 0) &&
	 (statflag == -1) &&
         (staterrno == ENOENT) &&
	 (stat(immutablebbname, &sinkstatbuf) >= 0) &&
	 ((statflag = stat(tempbuf, &st)) == -1) ) 
    {   
      /*newval==0 means that the public bboard directory probably
        contains no messages.  
        The other conditions above are heuristic to check if file server is up
        but newval file does not exist, by checking to see if
	the public bboard directory can be stat'ed.*/
      if (verbose) { printf("Purging .lastlink file\n"); fflush(stdout); }
      unlink(lastlinkname);      
    } else {
      WriteIntToFile(lastlinkname, newval);
      if (statflag == -1) bzero((char*)&st, sizeof(st));
      chmod(lastlinkvsname, 0755);
      vsfile = fopen(lastlinkvsname, "w");
      if (vsfile != NULL) {
        fprintf(vsfile, "%u\n%u\n%u\n%u\n", 
          st.st_ino, st.st_ctime, st.st_mtime, st.st_size);
        fclose(vsfile);
      }
    }
}



/***************************************************************/
/* SetMinMsg();                                                */
/*                                                             */
/*     The minimum message we will consider in the scan        */
/* of the remote directory must be greater than the image      */
/* of the largest number we found here and the message         */
/* number found in .lastlink, if any.  When this is called,    */
/* the number of the largest message in the directory is       */
/* in maxmsg.                                                  */
/***************************************************************/

static void SetMinMsg()
{
    int nch, linkval;
    stream linkfile;
    stream vsfile;
    char *slash;
    static struct stat mystbuf;

    minmsg = 0;
    if (maxmsg != 0) {

	/*Delete dangling symbolic link(s).  Start looking for them
	  at maxmsg and precede by decreasing numbers.  Stop when
	  find first non-dangling.*/
        while (maxmsg > 0) {
	  sprintf(tempbuf, "%s/%d", folder, maxmsg);	
	  if (DanglingMsgName(tempbuf) == DANG_SURE) {
	    /*Delete the dangling symbolic link.  Also delete the
	      .lastlink file in case it reflects the dangling symbolic
	      link.  Remove this message from the msgtable array, so that
	      it won't show up in the new .inodecache file.  And delete
	      the .inodecache file now, in case we error exit before we
	      get as far as writing the new one.*/
	    unlink(lastlinkname);
	    VAPtrSet(&msgtable, maxmsg, NULL);
	    unlink(cachename);
	    fprintf(stderr, "llscan: Purging dangling symbolic link '%s'\n", 
	      tempbuf);
	    if (unlink(tempbuf) < 0) error("Delete failed - %M");
	    maxmsg--;
	    madechanges = TRUE;
	  } else {
	    break;
	  }
	} /*while*/

	sprintf(tempbuf, "%s/%d", folder, maxmsg);
	nch = readlink(tempbuf, linkbuf, MAXPATHLEN);

        /* was if (nch <= 0) return; */

        if (nch == 0) {
	  fprintf(stderr, "Error: llscan: zero-length symbolic link '%s',\n",
	    tempbuf);
	  fprintf(stderr, "  will try to repair by deleting it.\n");
	  /*Also delete .lastlink file since it may reflect the 
	    bogus symlink: */
	  madechanges = TRUE;
	  unlink(lastlinkname);
	  if (unlink(tempbuf) < 0) error("Delete failed - %M");
	  error("llscan: exiting");
	} else if (nch < 0) {
	  if (lstat(tempbuf, &mystbuf) < 0) {
  	    error("llscan: cannot lstat alleged symbolic link '%s' - %M", 
	      tempbuf);
          } else if ((mystbuf.st_mode & S_IFMT) != S_IFLNK) {
	    fprintf(stderr,
	      "Error: llscan: file '%s' is not\n", tempbuf);
	    fprintf(stderr,
"  a symbolic link.  You will need to rm it by hand, or mv it to a\n");
            fprintf(stderr, 
"  non-numeric name -- until you do, llscan will fail.\n");
	    unlink(lastlinkname);
	    error("llscan: exiting");
	  } else error("llscan: cannot read symbolic link '%s' - %M", tempbuf);
	} /*if*/

	linkbuf[nch] = '\0';
	slash = (char*) rindex(linkbuf, '/');
	if ((slash == NULL) || ((minmsg = atoi(slash+1)) == 0)) {
	  fprintf(stderr, 
	    "Error: llscan: symbolic link '%s' has contents '%s'\n",
	    tempbuf, linkbuf);
          fprintf(stderr,
	   "  which are malformed: contents should end with a '/' followed\n");
	  fprintf(stderr, 
	   "  by a number.  You will need to delete this symbolic link\n");
	  fprintf(stderr,
           "  by hand -- until you do, llscan will fail.\n");
	  unlink(lastlinkname);
	  error("llscan: exiting");
	} /*if*/
    }

/*Added and removed 26-Jun-92
* Don't do this: it breaks the purge command in Postcard
* when messages to keep is zero:
*          if (purge) unlink(lastlinkname);  
*/

    linkfile = fopen(lastlinkname, "r");
    if (linkfile == NULL) {
      if (verbose) { 
        printf("Could not open .lastlink file.\n");
        fflush(stdout);
      }
      maxlink = minmsg;
      return;
    }
    if (fscanf(linkfile, "%d\n", &linkval) == 1) {
      sprintf(tempbuf, "%s/%d", immutablebbname, linkval);
      if (stat(tempbuf, &mystbuf) >= 0) {
        int ino, ct, mt, sz;
        vsfile = fopen(lastlinkvsname, "r");
        if (vsfile != NULL) {
	  if ( (fscanf(vsfile, "%u%u%u%u", &ino, &ct, &mt, &sz) == 4) &&
	       (ct == mystbuf.st_ctime) &&
	       (sz == mystbuf.st_size) )
	  {
	    if (verbose) { 
	      printf("Honoring .lastlinkvs file\n"); 
	      fflush(stdout);
	    }
	    if (linkval > minmsg) minmsg = linkval;
	  } else if (verbose) {
	    printf("Mismatched .lastlink version.\n");
	    fflush(stdout);
          }
	  fclose(vsfile);
	} else {
	  if (verbose) {
	    printf("Cannot open .lastlinkvs file, ignoring.\n");
	    fflush(stdout);
          }
	  if (linkval > minmsg) minmsg = linkval;
	}
      } else if (verbose) {
        printf("Cannot stat '%s', ignoring .lastlink\n",  tempbuf);
	fflush(stdout);
      }
    }
    fclose(linkfile);
} /*SetMinMsg*/



/***************************************************************/
/* SetBBoardName();                                            */
/*                                                             */
/*     If the .bblink file does not contain a leading slash,   */
/* calculate the name of the bboard by taking the current      */     
/* folder name, substituting all dots with slashes, and        */
/* prepending the environment variable NEWSDROP (defaulting    */
/* to /usr/spool/news).                                        */
/*     If the .bblink does contain leading slash, simply       */
/* take that as the name of the directory.                     */
/*                                                             */
/*     The name of the bboard directory is stored in           */
/* bboardname, and the address of the terminating null is      */
/* stored in bboardptr.                                        */
/***************************************************************/

static void SetBBoardName()
{
    char *src, *dst;
    int len;
    string bbpath;
    stream bbfile;
    bool fullpath;
    string newsdrop;
    int fd;

    fullpath = FALSE;
    newsdrop = getenv("NEWSDROP");
    if (newsdrop == NULL) newsdrop = DEFAULTNEWSDROP;
    strcpy(bboardname, newsdrop);
    dst = bboardname + strlen(bboardname);
    *dst++ = '/';
    bbfile = fopen(sconc(GetFolder(foldername), "/.bblink"), "r");
    if (bbfile == NULL) {
	bbpath = foldername;
    } else {
	(void) fgets(tempbuf, MAXPATHLEN, bbfile);
	tempbuf[strlen(tempbuf) - 1] = '\0';
        bbpath = scopy(tempbuf);
        fullpath = bbpath[0] == '/';
    }
    if (! fullpath) {
        for (src = bbpath; *src; src++)
    	    *dst++ = (*src == '.') ? '/' : *src;
        *dst = '\0';
    } else {
        strcpy(bboardname, bbpath);
        len = strlen(bboardname);
        bboardname[len] = '\0';
        dst = &bboardname[len];
    }
    bboardptr = dst;
    if (verbose) {
        printf("SetBBoardName='%s'\n", bboardname);
	fflush(stdout);
    }
    immutablebbname = scopy(bboardname);
    if (stat(bboardname, &sinkstatbuf) < 0) {
        error("llscan: cannot access (using stat) directory '%s' - %M",
	    bboardname);
    }
    fd = open(lastlinkvsname, (O_RDWR | O_CREAT), 0755);
    if (fd != -1) close(fd);
}



/***************************************************************/
/* AddBBLink(private, public);                                 */
/*                                                             */
/*     Makes a symbolic link in the current folder to the      */
/* actual message in the public folder.  The private name      */
/* is also added to the scan and directory lists.              */
/***************************************************************/

static void AddBBLink(private, public)
int private, public;
{
    hashentryptr hp;
    int err;
    static char tbuf[MAXPATHLEN];
    int nch;

    madechanges = TRUE;
    sprintf(linkbuf, "%s/%d", folder, private);
    sprintf(bboardptr, "/%d", public);
    if (lstat(bboardname, &sinkstatbuf) < 0) {
      error("llscan: cannot lstat file '%s' - %M", bboardname);
    }
    if (symlink(bboardname, linkbuf) < 0) {
      err = errno;
      fprintf(stderr, 
        "Error: llscan: cannot create symbolic link '%s' - %s\n",
	linkbuf, errno2str(err));
      /*Try to cleanup Ultrix problem that it can leave zero-length
        symbolic links lying around when it's out of space:  */      
      nch = readlink(linkbuf, tbuf, MAXPATHLEN);
      if (nch == 0) unlink(linkbuf);
      error("llscan: exiting");
    } /*if*/

    if (lstat(linkbuf, &statbuf) < 0) {
      error("llscan: cannot lstat symbolic link '%s' - %M", linkbuf);
    }

    if (DanglingMsgName(linkbuf) == DANG_NOT) {
      if (cacheflag) AddScanArg(private);
      hp = (hashentryptr) getmem(sizeof(hashentryrec));
      IntTablePut(hashtable, (int) statbuf.st_ino, hp);
      hp->inode = statbuf.st_ino;
      hp->header[0] = 0;
      VAPtrSet(&msgtable, private, hp);
      ++nmsgs;  
    } else {
      if (verbose) {
        printf("Ignoring (new) dangling symbolic link '%s'\n", linkbuf);
	fflush(stdout);
      }
    }

}



/***************************************************************/
/* BBSelect(dp)                                                */
/* BBCompare(dp)                                               */
/*                                                             */
/*     These functions are passed to the scandir routine       */
/* when enumerating the remote mail directory.  BBSelect       */
/* chooses only valid messages with numbers greater than       */
/* minmsg (note that more than three digits is acceptable      */
/* here).  BBCompare sorts them numerically.                   */
/***************************************************************/

static int BBSelect(dp)
struct direct *dp;
{
    return (IsAMessage(dp->d_name) && atoi(dp->d_name) > minmsg);
}

static int BBCompare(dp1, dp2)
struct direct **dp1, **dp2;
{
    return (atoi((*dp1)->d_name) - atoi((*dp2)->d_name));
}



/***************************************************************/
/* InitHomeMailDir()
/*
/*     Initializes the global variables "home" and "mailDir". 
/* Currently, this is done explicitly here, but should be 
/* rewritten to use the .mh_profile file.
/***************************************************************/

static void InitHomeMailDir()
{
    home = getenv("HOME");
    if (home == NULL) {
        string user;
        user = getenv("USER");
        if (user == NULL) error("Can't find user name");
	home = sconc("/udir/", user);
    } /*if*/
    mailDir = sconc(home, "/Mail");
    if (verbose) {
      printf("home='%s' mailDir='%s'\n", home, mailDir);
      fflush(stdout);
    }
} /*InitHomeMailDir*/



/***************************************************************/
/* dirname = GetFolder(name)                                   */
/*                                                             */
/*     Returns the name of the directory corresponding to      */
/* the specified folder name.                                  */
/***************************************************************/

static string GetFolder(name)
string name;
{
    sprintf(tempbuf, "%s/%s", mailDir, name);
    if (verbose) { printf("folderDir='%s'\n", tempbuf); fflush(stdout); }
    return (scopy(tempbuf));
}



/***************************************************************/
/* flag = FolderIsBBoard(name);                                */
/*                                                             */
/*     Tests to see whether a folder is a bulletin board.      */
/***************************************************************/

static bool FolderIsBBoard(name)
string name;
{
    return (index(name, '.') != NULL ||
	    access(sconc(GetFolder(name), "/.bblink"), R_OK) == 0);
}



/***************************************************************/
/* flag = IsAMessage(name);                                    */
/*                                                             */
/*     Tests whether the specified name is a legal message     */
/* number (i.e., string of digits).                            */
/***************************************************************/

static bool IsAMessage(name)
string name;
{
    char *cp;
    char c;

    cp = name;
    while (c = *cp++)
	if (!isdigit(c)) return (FALSE);
    return (TRUE);
}



/***************************************************************/
/* warning(str, [args]);                                       */
/*                                                             */
/*     This entry is like error except that it continues.      */
/* If the -w option is used, no warnings are generated.        */
/***************************************************************/

static void warning(string msg, ...) 
{
  va_list args;

  if (warnings) {
    va_start(args, msg);
    fprintf(stderr, "llscan: Warning: ");
    vfprintf(stderr, msg, args);
    va_end(args);
    fprintf(stderr, "\n");
  }
}

static const char *errno2str(err)
{
    static char buf[50];
    if (err >= sys_nerr) {
      sprintf(buf, "Unknown_Error_Code_%d", err);
      return scopy(buf);
    } else return sys_errlist[err];
} /*errno2str*/



/*
Edit History:  (started 12-Nov-90

12-Nov-90 hisgen:  
  In calling mh programs scan and inc, check for error by using
pclose.  But whether or not to bomb on error is controlled by an
environment variable.  For now, we default to not bombing, 
because too many normal cases cause inc to error exit, in
particular, mailbox busy due to competing delivery process.
I'm leaving this code in even though it's normally off because
it could help with trouble-shooting.  
16-Jan-91: Changed to normally on.


12-Nov-90 hisgen:
  Andrew Birrell is planning to release a PostCard that does
implicit inc's in background.  This increases the chances
that an unwitting user will run lauralee at home while leaving
PostCard running at work, making it much more likely that
two conflicting llscan's will be running simultaneously.
To get around this problem, I've change various file writing
code to always write a temporary file and then rename(2)
it atomicly.  This strategy also relies on the fact that 
llscan is "self-repairing" -- simply running it again will
tend to fix any problems.
  The routines I've changed that write files are DumpCache,
DumpCurMsg, and DumpMaxLink.  I've added additional support
routines GetMyHostName, dorename, mktempname, and WriteIntToFile.


12-Nov-90 hisgen:
  Just before exiting, I'm now calling utimes to set the mtime
(and atime) of the .inodecache file.  The motivation for
doing this is that after llscan makes its last write to the
.inodecache file, it still does directory updates after that
point.  PostCard uses the mtime of the directory versus the
mtime of the .inodecache file to guess whether it needs to call
llscan.  Thus, calling utimes will tend to make PostCard
happier.

Dec-90 hisgen:
  Added -l switch and islauralee boolean.  The idea is that
some code is executed only if the calling program really
is lauralee.   As of now, the only use of this feature is
to tell whether some program other than lauralee has
run llscan in a folder -- if so, it leaves behind a .llscan
file.  lauralee tests for the existence of this file.
The motivation for all of this is better interaction of
lauralee with Postcard's background activity.

6-Apr-92 hisgen:
Added NEWSDROP environment variable.  Defaults to "/usr/spool/news".

*/
