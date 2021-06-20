/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/***************************************************************/
/* File: file.c                                                */
/* Last modified on Thu Feb 23 10:26:11 PST 1995 by kalsow     */
/*      modified on Fri May 17 13:07:17 1991 by hisgen         */
/*      modified on Wed Feb  4 17:08:20 1987 by roberts        */
/* ----------------------------------------------------------- */
/*      File operations.  See documentation in file.h          */
/***************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "SRCstdlib.h"
#include "strlib.h"
#include "file.h"

typedef string (*strfn)();

/***************************************************************/
/* Local function declarations                                 */
/***************************************************************/

static string checkexists(/* name */);



/***************************************************************/
/* str = root(filename);                                       */
/* str = extension(filename);                                  */
/* str = head(filename);                                       */
/* str = tail(filename);                                       */
/*                                                             */
/*     Each of these routines returns a component of a file    */
/* name and are equivalent to the C shell substitution         */
/* characters r, e, h, and t, respectively.                    */
/***************************************************************/

string root(filename)
string filename;
{
    char *dotpos;

    dotpos = (char*) rindex(filename, '.');
    if (dotpos != NULL && index(dotpos, '/') != NULL) dotpos = NULL;
    if (dotpos == NULL)
	return (scopy(filename));
    else
        return (substr(filename, 0, (dotpos - filename) - 1));
}

string extension(filename)
string filename;
{
    char *dotpos;

    dotpos = (char*) rindex(filename, '.');
    if (dotpos != NULL && index(dotpos, '/') != NULL) dotpos = NULL;
    if (dotpos == NULL)
	return ("");
    else
        return (scopy(dotpos + 1));
}

string head(filename)
string filename;
{
    char *slashpos;

    slashpos = (char*) rindex(filename, '/');
    if (slashpos == NULL)
	return (scopy(filename));
    else
        return (substr(filename, 0, (slashpos - filename) - 1));
}

string tail(filename)
string filename;
{
    char *slashpos;

    slashpos = (char*) rindex(filename, '/');
    if (slashpos == NULL)
	return (scopy(filename));
    else
        return (scopy(slashpos + 1));
}



/***************************************************************/
/* newname = defext(oldname, ".xxx")                           */
/*                                                             */
/*     The defext routine adds an extension to a file name     */
/* if none already exists.  Alternatively, if the extension    */
/* field begins with a *, any old extension in the first       */
/* filename is replaced with the given extension.              */
/*                                                             */
/*     defext(filename, ".xxx")   --  add .xxx if no ext       */
/*     defext(filename, "*.xxx")  --  force .xxx as ext        */
/*                                                             */
/* Note:  defext returns a pointer to dynamically-allocated    */
/* string storage which is never freed.  This is necessary     */
/* to ensure safety on multiple calls in a single statement.   */
/***************************************************************/

string defext(filename, ext)
string filename, ext;
{
    char c, *cp;
    char *xp;
    bool forceext;

    if (forceext = (ext[0] == '*')) ext++;
    xp = NULL;
    for (cp = filename; c = *cp; cp++) {
	switch (c) {
	    case '/' : case ':' : xp = NULL; break;
	    case '.' : xp = cp;
	}
    }
    if (xp == NULL) {
	forceext = TRUE;
	xp = cp;
    }
    if (forceext)
	return (sconc(substr(filename, 0, xp-filename-1), ext));
    else
	return (scopy(filename));
}



/***************************************************************/
/* stream = pathopen(path, filename, mode);                    */
/*                                                             */
/*     The pathopen routine is used to open files using a      */
/* search path similar to that used, for example, by csh       */
/* in searching for a command.  The pathopen routine has       */
/* the same structure as fopen in the standard library and     */
/* the filename and mode arguments are the same as in that     */
/* call.  The path argument consists of a list of directories  */
/* which are prepended to the filename, unless the filename    */
/* begins with either a / or a ~.  The directories in the      */
/* list are separated by colons as in the definition of the    */
/* PATH environment variable.  White space and empty fields    */
/* are ignored to simplify formatting of paths in a definition */
/* file.                                                       */
/*                                                             */
/*     After each directory name has been added, the           */
/* pathopen function performs ~ expansion in the same form as  */
/* csh.  The path argument may be NULL, in which case no       */
/* directories are prepended.  This is useful if ~ expansion   */
/* is the only required function.                              */
/*                                                             */
/*     The pathopen function returns an open stream to         */
/* the indicated file, or NULL, if no existing file is         */
/* found.                                                      */
/***************************************************************/

stream pathopen(path, filename, mode)
string path, filename, mode;
{
    return ((stream) _mappath((strfn) fopen, path, filename, mode));
}



/***************************************************************/
/* newname = pathfind(path, filename);                         */
/*                                                             */
/*     The pathfind routine is similar to pathopen, except     */
/* that it does not try to open the file but instead returns   */
/* the full name of the first file that exists along the path, */
/* or NULL if none exist.                                      */
/***************************************************************/

string pathfind(path, filename)
string path, filename;
{
    return (_mappath(checkexists, path, filename, (string) NULL));
}

/***************************************************************/
/* Internal routine to determine if a file exists              */
/***************************************************************/

static string checkexists(name)
string name;
{
    static struct stat statbuf;

    return ((stat(name, &statbuf) == 0) ? name : NULL);
}



/***************************************************************/
/* newname = expandtilde(oldname);                             */
/*                                                             */
/*     Given a filename, returns it after performing tilde     */
/* expansion.                                                  */
/***************************************************************/

string expandtilde(name)
string name;
{
    string slashpos, homedir, newname;
    struct passwd *pw;

    if (*name != '~') return (name);
    slashpos = (char*) index(name, '/');
    if (slashpos == NULL) slashpos = name + strlen(name);
    if (slashpos - name == 1) {
	homedir = getenv("HOME");
	if (homedir == NULL) homedir = getpwuid(getuid())->pw_dir;
    } else {
	homedir = substr(name, 1, slashpos-name-1);
	pw = getpwnam(homedir);
        if (pw == NULL) {
	    fprintf(stderr, "Error [cc]: No such user -- %s\n", homedir);
	    exit(1);
	}
	homedir = pw->pw_dir;
    }
    newname = sconc(homedir, slashpos);
    return (newname);
}



/***************************************************************/
/* s = _mappath(fn, path, filename, arg);                      */
/*                                                             */
/*     Maps the string function fn over each path/filename     */
/* combination, stopping when fn(name, arg) returns non-null.  */
/* That value is returned.  If the function always returns a   */
/* NULL value for each element in the path, NULL is returned   */
/* from _mappath.                                              */
/***************************************************************/

string _mappath(fn, path, filename, arg)
strfn fn;
string path, filename, arg;
{
    char *cp;
    char c;
    string start, finish, localpath, tempname, result;

    if (filename == NULL) filename = "";
    c = filename[0];
    if (path == NULL || path[0] == '\0' || c == '~' || c == '/')
	return ((*fn)(expandtilde(filename), arg));
    localpath = sconc(path, ":");
    result = NULL;
    start = localpath;
    while (result == NULL && (finish = (char*) index(start, ':')) != NULL) {
	while (isspace(*start)) start++;
        for (cp = finish-1; cp > start && isspace(*cp); cp--);
	*++cp = '\0';
	if (start != finish || strlen(localpath) == 1) {
	    tempname = sconc(start, sconc("/", filename));
	    result = (*fn)(expandtilde(tempname), arg);
	}
	start = finish+1;
    }
    return (result);
}
