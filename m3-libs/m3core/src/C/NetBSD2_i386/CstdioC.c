/* Copyright (C) 1994, Digital Equipment Corporation */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */
/*                                                            */
/* Last modified on Mon Oct 17 09:17:31 PDT 1994 by kalsow    */
/*      Olaf Wagner 16.09.1994                                */
/*      Phil Nelson 17.05.1999                                */

#include <stdio.h>

int nbsd_feof(FILE *f){
   return feof(f);
};

int nbsd_getc(FILE *f){
   return fgetc(f);
};

int nbsd_ungetc(int c, FILE *f){
   return ungetc(c, f);
};

int nbsd_putc(int c, FILE *f){
   return fputc(c, f);
};

int nbsd_fflush(FILE *f){
   return fflush(f);
};

FILE* nbsd_fdopen(int fildes, char *mode){
   FILE* f;
   f = fdopen(fildes, mode);
   if (f == NULL){
       fprintf(stderr, "fd: %d, mode: %s\n", fildes, mode);
       perror("fdopen failed ");
   }
   return f;
}

int nbsd_fclose(FILE *f){
   return fclose(f);
};
