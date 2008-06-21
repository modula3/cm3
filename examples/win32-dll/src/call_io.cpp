
/*
  This sample shows how to call Modula-3 standard library routines
 */
extern "C" void* M3toC__StoT(char*);
extern "C" void IO__Put(void* p, void *txt);
extern "C" void IO__PutInt(int n, void* p);
extern "C" void IO__PutReal(float f, void* p);

extern "C" int m3main (int, char**, char**);


int main(int argc, char** argv, char**argp)
{
  int i;

  /* Initialize Modula-3 DLL's */
  m3main(argc, argv, argp);

  IO__Put(M3toC__StoT("\nThis program demonstrates how to initialize "
                      "and use Modula-3 DLL's\n\n"), 0);

  IO__Put(M3toC__StoT("Program: "), 0);
  IO__Put(M3toC__StoT(argv[0]), 0);
  IO__Put(M3toC__StoT("\n"), 0);

  IO__Put(M3toC__StoT("Number of arguments: "), 0);
  IO__PutInt(argc-1, 0);
  IO__Put(M3toC__StoT("\n"), 0);

  for ( i = 1; i < argc; i++ )
  {
    IO__Put(M3toC__StoT(argv[i]), 0);
    IO__Put(M3toC__StoT(" "), 0);
  }

  IO__Put(M3toC__StoT("\n"), 0);

  IO__Put(M3toC__StoT("Calling IO.PutReal(12345.6789) => "), 0);
  IO__PutReal((float)12345.6789, 0);
  
  IO__Put(M3toC__StoT("\n\nGoog bye!\n"), 0);

  return 0;
}