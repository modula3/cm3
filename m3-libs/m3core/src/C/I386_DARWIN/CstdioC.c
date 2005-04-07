/* Copyright according to COPYRIGHT-CMASS. */
/* FIXME: copied from FreeBSD3 target. Probably needs to be changed. */

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
