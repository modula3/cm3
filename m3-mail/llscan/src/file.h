/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/***************************************************************/
/* File: file.h                                                */
/* Last modified on Thu Jun  2 15:39:47 PDT 1994 by kalsow     */
/*      modified on Fri May 17 13:04:06 1991 by hisgen         */
/*      modified on Fri Jan 30 15:50:26 1987 by roberts        */
/* ----------------------------------------------------------- */
/*     Provides several standard procedures for working with   */
/* files:                                                      */
/*                                                             */
/*         root(filename)                                      */
/*         extension(filename)                                 */
/*         head(filename)                                      */
/*         tail(filename)                                      */
/*         defext(filename, ".xxx")                            */
/*         pathopen(path, filename, mode)                      */
/*         pathfind(path, filename)                            */
/*                                                             */
/* Each of these procedures is described separately below.     */
/***************************************************************/

#ifndef _file_h
#define _file_h
#ifndef  _SRCstdlib_h
#include <SRCstdlib.h>
#endif

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

char *root(/* filename */);
char *extension(/* filename */);
char *head(/* filename */);
char *tail(/* filename */);



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

char *defext(/* filename, ext */);



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

stream pathopen(/* path, filename, mode */);



/***************************************************************/
/* newname = pathfind(path, filename);                         */
/*                                                             */
/*     The pathfind routine is similar to pathopen, except     */
/* that it does not try to open the file but instead returns   */
/* the full name of the first file that exists along the path, */
/* or NULL if none exist.                                      */
/***************************************************************/

string pathfind(/* path, filename */);



/***************************************************************/
/* newname = expandtilde(oldname);                             */
/*                                                             */
/*     Given a filename, returns it after performing tilde     */
/* expansion.                                                  */
/***************************************************************/

string expandtilde(/* filename */);



/***************************************************************/
/* Internal _mappath which may be useful to some clients.      */
/* See code for documentation.                                 */
/***************************************************************/

string _mappath(/* fn, path, filename, arg */);

#endif
