/* Copyright (C) 1989, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Mon Jan  9 10:18:07 PST 1995 by kalsow  */
/*      modified on Tue Oct 13 17:23:43 PDT 1992 by muller  */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#define TRUE 1
#define FALSE 0

FILE *command_file_stack[100];
long nb_command_files = 0;

long verbose_mode = FALSE;

char text[100] = "";
FILE *code;
FILE* output_file;
/*****
long current_line, line, count, p, number;
char c;
char name [100];
****/

char error_message[1000];


typedef struct _proc {
  char *proc_name;
  long  count;
  struct _proc *next;
} proc_struct, *proc;

typedef struct _collection {
  char *file_name;
  long time_stamp;
  long first_line;
  long data_size;
  long *data_points;
  proc  my_procs;
  long nb_procs;
  struct _collection *next;
} collection_struct, *collection;


collection collections = NULL;

char * program_name = NULL;
const char * source_path = NULL;

long database_loaded = FALSE;

char *search_components [500] = {NULL};
long nb_components = 0;

void warning (void)
{
  fprintf (stderr, "%s: %s\n", program_name, error_message);
}

void error (void)
{
  warning ();
  exit (1);
}

char *safe_malloc (size_t size)
{
  char *p = (char *)malloc (size);
  if (p == NULL) {
    sprintf (error_message,
             "Cannot malloc %ld byte%s", size, (size==1?"":"s"));
    error (); }
  return (p);
}

void augment_source_path (const char * path)
{
  char *n = safe_malloc (strlen (path) + 1);
  strcpy (n, path);
  n = strtok (n, ":");

  while (n != NULL) {
    search_components [nb_components++] = n;
    n = strtok (NULL, ":"); }
  search_components [nb_components] = NULL;
}


FILE* locate_and_open (const char* file_name, const char* mode)
{
  FILE* f = NULL;
  long i = 0;
  char full_name[MAXPATHLEN+1];

  while (f == NULL & search_components[i] != NULL) {
    strcpy (full_name, search_components[i]);
    strcat (full_name, "/");
    strcat (full_name, file_name);
    f = fopen (full_name, mode);
    i++;
  }
  return (f);
}

collection find_collection (const char* name)
{
  collection c;

  for (c = collections;
       c != NULL && strcmp (name, c->file_name) != 0;
       c = c->next) ;

  return (c);
}
  
proc find_proc (const char* name, proc procs)
{
  while (procs != NULL && strcmp (name, procs->proc_name) != 0) {
    procs = procs->next; }

  return (procs);
}


void read_string_tail (long file, long len)
{
  long x, i = len + sizeof (long) - 1;
  i /= sizeof (long); 
  i *= sizeof (long);
  i -= len;
  if (i > 0) read (file, &x, i);
}

                
void read_coverage_data (const char* data_file) 
{
  long time_stamp;
  long name_length;
  char *source_name;
  struct stat stat_buf;
  long data_size;
  long chunk_size;
  long first_line;
  long *data_points;
  long nb_procs;
  collection c;
  char *proc_name;
  long proc_count;
  long f = open (data_file, O_RDONLY, 0);    
  FILE* fs;

  if (verbose_mode) {
    fprintf (stderr, "Reading database file %s ...\n", data_file);
  }

  if (f < 0) {
    sprintf (error_message, "cannot read coverage file %s", data_file); 
    error (); }

  while (read (f, &chunk_size, sizeof (long)) != 0) {

    read (f, &time_stamp, sizeof (long));

    read (f, &name_length, sizeof (long));

    source_name = (char *) safe_malloc (name_length+1);
    read (f, source_name, name_length);
    read_string_tail (f, name_length);
    source_name[name_length] = 0;
    if (verbose_mode) { fprintf (stderr, "... %s\n", source_name); }

    fs = locate_and_open (source_name, "r");
    if (fs == NULL || fstat (fileno (fs), &stat_buf) == -1) {
      sprintf (error_message,
               "cannot obtain stats about %s", source_name);
      error ();
    } else {
      fclose (fs);
    }

    if (/*** stat_buf.st_mtime ***/ 0 != time_stamp) {
      sprintf (error_message, "wrong timestamp for %s", source_name);
      warning ();
    }

    read (f, &first_line, sizeof (long));

    read (f, &data_size, sizeof (long));
    data_points = (long *) safe_malloc (data_size * sizeof (long));
    read (f, data_points, data_size * sizeof (long));

    if ((c = find_collection (source_name)) != NULL) {
      if (c->time_stamp != time_stamp) {
        sprintf (error_message, 
                 "different timestamps for %s", source_name);
        warning (); }
      if (c->first_line != first_line) {
        sprintf (error_message, 
           "new and old data have different source lines for %s", source_name);
        warning (); }
      if (c->data_size != data_size) {
        sprintf (error_message, 
                 "new and old data of different sizes for %s", source_name);
        warning (); }
      { long i;
        for (i = 0; i < data_size; i++) {
          if (data_points[i] >= 0) {
            if (c->data_points[i] >= 0) {
              c->data_points[i] += data_points[i]; }
            else {
              c->data_points [i] = data_points[i]; }}}}
      free (source_name); 
      free (data_points); }
    else {
      c = (collection) safe_malloc (sizeof (collection_struct));
      c->file_name = source_name;
      c->time_stamp = time_stamp;
      c->data_points = data_points;
      c->first_line = first_line;
      c->data_size = data_size;
      c->next = collections;
      c->my_procs = NULL;
      c->nb_procs = 0;
      collections = c; }

    read (f, &nb_procs, sizeof (long));

    while (nb_procs-- > 0) {
      proc p;

      read (f, &name_length, sizeof(long));

      proc_name = (char *) safe_malloc (name_length+1);
      read (f, proc_name, name_length);
      read_string_tail (f, name_length);
      proc_name[name_length] = 0;

      read (f, &proc_count, sizeof (long));
      if ((p = find_proc (proc_name, c->my_procs)) != NULL) {
        p->count += proc_count; 
        free (proc_name); }
      else {
        p = (proc) safe_malloc (sizeof (proc_struct));
        p->proc_name = proc_name;
        p->next = c->my_procs;
        c->my_procs = p;
        c->nb_procs ++;
        p->count = proc_count; }}}
  close (f);
  database_loaded = TRUE;

  if (verbose_mode) {
    fprintf (stderr, "... done\n"); }
}

void ensure_that_database_is_loaded (void)
{
  const char* coverage_file;

  if (! database_loaded) {
    if (verbose_mode) {
      fprintf (stderr, "we need a database and none has been loaded\n"); }
    coverage_file = getenv ("COVERAGE_DATABASE");
    if (coverage_file == NULL) {
      coverage_file = "coverage.out"; }
    read_coverage_data (coverage_file); }
}


void write_string_tail (long file, long len)
{
  long x = 0, i = len + sizeof (long) - 1;
  i /= sizeof (long); 
  i *= sizeof (long);
  i -= len;
  if (i > 0) write (file, &x, i);
}


void write_coverage_data (const char* data_file)
{
  long f = open (data_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
  collection c;
  proc p;

  if (verbose_mode) {
    fprintf (stderr, "writing the database to %s ...\n", data_file);
  }

  if (f == 0) {
    sprintf (error_message, "cannot write coverage file %s", data_file);
    error ();
  }

  for (c = collections;
       c != NULL;
       c = c->next) {
    if (verbose_mode) {
      fprintf (stderr, "... %s\n", c->file_name); }

    { long i = 0;  /* chunk size */
      write (f, &i, sizeof (long));
    }

    write (f, &(c->time_stamp), sizeof (long));

    { long i = strlen (c->file_name);
      write (f, &i, sizeof (long));
      write (f, c->file_name, i);
      write_string_tail (f, i);
    }

    write (f, &(c->first_line), sizeof (long));

    write (f, &(c->data_size), sizeof (long));

    write (f, c->data_points, c->data_size * sizeof (long));

    write (f, &c->nb_procs, sizeof (long));

    for (p = c->my_procs;  p != NULL;  p = p->next) {
      long i = strlen (p->proc_name);
      write (f, &i, sizeof (long));
      write (f, p->proc_name, i);
      write_string_tail (f, i);
      write (f, &p->count, sizeof (long)); }}

  close (f);
  if (verbose_mode) {
    fprintf (stderr, "... done\n"); }
}

void show_procs (const char* file_name) 
{
  collection c = find_collection (file_name);
  proc p;

  if (c == NULL) {
    sprintf (error_message,
             "no data collected for %s", file_name);
    error (); }

  fprintf (output_file,
           "*************** PROCEDURE COVERAGE OF %s\n\n", file_name);

  for (p = c->my_procs; p != NULL; p = p->next) {
    switch (p->count) {
      case 0:  fprintf (output_file, "   no calls"); break;
      case 1:  fprintf (output_file, "    1 call "); break; 
      default: fprintf (output_file, "%5ld calls",  p->count);   break; }
    fprintf (output_file, " to %s\n", p->proc_name); }
  fprintf (output_file, "\n");
}

long read_line (FILE* f, char* buff)
{
  long p = 0;
  char c;

  c = getc (f);
  while (c != EOF && c != '\n') {
    buff [p++] = c;
    c = getc (f); }
  buff [p] = '\0';
  return ((c != EOF) || (p != 0));
}

void show_lines (char* source_file)
{
  FILE *code;
  long source_line, data_line;
  char line_buffer [500];
  collection c;

  code = locate_and_open (source_file, "r");
  if (code == NULL) {
    sprintf (error_message,
             "cannot open source file %s", source_file);
    error (); }

  if ((c = find_collection (source_file)) == NULL) {
    sprintf (error_message,
             "no data collected for %s", source_file); 
    error (); }

  fprintf (output_file, "*************** COVERAGE OF %s\n\n", source_file);

  source_line = data_line = 0;
  while (read_line (code, line_buffer)) {
    source_line ++;
    if ((source_line < c->first_line)
      || (c->data_size + c->first_line <= source_line))
      { fprintf (output_file, "       %s\n", line_buffer); }
    else {
      if (c->data_points [data_line] >= 0) {
        fprintf (output_file, 
                 "%6ld  %s\n", c->data_points[data_line], line_buffer); }
      else {
        fprintf (output_file, "       %s\n", line_buffer); }
      data_line++; }}

  fprintf (output_file, "\n");
  fclose (code);
}

char* basename (char* n)
{
  char* b = strrchr (n, '/');
  if (b == NULL) {
    return (n); }
  else {
    return (b+1); }
}


long arg_c, argc;
char **arg_v, **argv;
char *file_argv[100];

typedef enum {
#define COMMAND(letter,name,args) name,
#include "analyze_coverage.h"
  DUMMY,
  UNKNOWN } command_list;

command_list command;

enum CommSrc { FROM_ARGS, FROM_FILE } command_source = FROM_ARGS;

void init_command (long c, char **v)
{
  command_source = FROM_ARGS;
  argc = c;
  argv = v;
}

void get_command (void) 
{
  enum { NONE, ONE, LIST } some_args;

  if (command_source == FROM_ARGS) {
    if (argc == 0) {
      command = QUIT;
      return; }
    if (argv[0][0] != '-') {
      sprintf (error_message, "commands should start with a '-' (%s)",
                  argv[0]);
      error (); }
    command = UNKNOWN;
    switch (argv[0][1]) {
      case '\0' : {
        command_source = FROM_FILE;
        command_file_stack [++nb_command_files] = stdin;
        break; }
#ifdef COMMAND
#undef COMMAND
#endif
#define COMMAND(letter,name,args) \
      case letter: { command = name; some_args = args; break; }
#include "analyze_coverage.h"
      default: 
        sprintf (error_message, "%s: unknown command", argv[0]);
        error (); }
    argc--; argv++;
    if (command != UNKNOWN) {
      switch (some_args) {
        case NONE: {
          arg_c = 0; break; }
        case ONE:  {
          if (argc >= 1) { 
            arg_c = 1; arg_v = argv;
            argc--; argv++; }
          else {
            sprintf (error_message, "not enough arguments for %s",
                        argv[-1]);
            error (); }
          break; }
        case LIST: {
          arg_c = 0;
          arg_v = argv;
          while (argc > 0 && (argv[0][0] != '-')) {
            arg_c++; argc--; argv++; }
          break; }}
      return; }}

  if (command_source == FROM_FILE) {
    char command_line[500];    
    char *command_char = NULL;
    arg_v = file_argv;
    while (command_char == NULL) {
      printf ("ca> ");
      fflush (stdout);
      if (! read_line (command_file_stack[nb_command_files], command_line)) {
        command = DUMMY;
        if (command_file_stack[nb_command_files] != stdin) {
          printf ("EOF\n"); 
          fclose (command_file_stack[nb_command_files]);}
        if (--nb_command_files == 0) {
          command_source = FROM_ARGS; }
        return; }
      if (command_file_stack[nb_command_files] != stdin) {
        printf ("%s\n", command_line); }
      command_char = strtok (command_line, " \t"); }

    switch (command_char[0]) {
#include "analyze_coverage.h"
      default:
        sprintf (error_message, "%s: unknown command", command_char);
        error (); }

    for (arg_c = 0;
         (arg_v[arg_c] = strtok (NULL, " \t")) != NULL;
         arg_c++); }
}

int main (int argc, char** argv)
{
  output_file = stdout;

  program_name = basename (argv[0]);
  init_command (argc-1, argv+1);

  source_path = getenv ("COVERAGE_PATH");
  if (source_path == NULL) {
    source_path = "."; }
  augment_source_path (source_path);


  while (TRUE) {
   get_command ();
   switch ((int)command) {
      case QUIT: {
        exit (0);
        break; }
      case DUMMY: {
        break; }
      case EXEC_FILE: {
        FILE *f = fopen (*arg_v, "r");
        if (f == NULL) {
          sprintf (error_message, "cannot open command file %s",
                      *arg_v);
          error (); }
        command_file_stack [++nb_command_files] = f;
        command_source = FROM_FILE;
        break; }      
      case READ_DATABASE: {
        while (arg_c-- > 0) {
          read_coverage_data (*arg_v++); }
        break; }
      case WRITE_DATABASE: {
        ensure_that_database_is_loaded ();
        while (arg_c-- > 0) {
          write_coverage_data (*arg_v++); }
        break; }
      case SHOW_LINES: {
        ensure_that_database_is_loaded ();
        while (arg_c-- > 0) {
          show_lines (basename (*arg_v++)); }
        break; }
      case SHOW_LINES_ALL: {
        collection c;
        ensure_that_database_is_loaded ();
        for (c = collections; c != NULL; c = c->next) {
          show_lines (c->file_name); }
        break; }
      case SHOW_PROCS: {
        ensure_that_database_is_loaded ();
        while (arg_c-- > 0) {
          show_procs (basename (*arg_v++)); }
        break; }
      case SHOW_PROCS_ALL: {
        collection c;
        ensure_that_database_is_loaded ();
        for (c = collections; c != NULL; c = c->next) {
          show_procs (c->file_name); }
        break; }
      case AUGMENT_SOURCE_PATH: {
        while (arg_c-- > 0) {
          augment_source_path (*arg_v++); }
        break; }
      case SELECT_OUTPUT: {
        FILE* f = fopen (*arg_v, "w"); 
        if (f == NULL) {
          sprintf (error_message,
                   "cannot open %s for output - %s", *arg_v,
                   "redirection ignored\n");
          warning (); }
        else {
          if (output_file != stdout) {
            fclose (output_file); }
          if (verbose_mode) {
            fprintf (stderr, "output file is now %s\n", *arg_v); }
          output_file = f; }
        break; }
      case VERBOSE_ON: {
        verbose_mode = TRUE;
        break; }}}

  /* exit (0); */
}
