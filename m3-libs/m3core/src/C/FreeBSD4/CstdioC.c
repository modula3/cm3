/* Copyright (C) 1994, Digital Equipment Corporation          */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */
/*                                                            */
/* Last modified on Mon Oct 17 09:17:31 PDT 1994 by kalsow    */
/*      Olaf Wagner 16.09.1994                                */

#include <stdio.h>

int fbsd_feof(FILE *f){
   return feof(f);
};

int fbsd_getc(FILE *f){
   return fgetc(f);
};

int fbsd_ungetc(int c, FILE *f){
   return ungetc(c, f);
};

int fbsd_putc(int c, FILE *f){
   return fputc(c, f);
};

int fbsd_fflush(FILE *f){
   return fflush(f);
};

FILE* fbsd_fdopen(int fildes, char *mode){
   FILE* f;
   f = fdopen(fildes, mode);
   if (f == NULL){
       fprintf(stderr, "fd: %d, mode: %s\n", fildes, mode);
       perror("fdopen failed ");
   }
   return f;
}

int fbsd_fclose(FILE *f){
   return fclose(f);
};
