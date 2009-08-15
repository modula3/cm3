

(* This simple program for mapping file extensions 
   (e.g., .m3, .c, .txt) to file kind, you learn about
   enumerations, arrays, and record, and constant initializers. *)

MODULE ExtensionLookup EXPORTS Main;
IMPORT IO, Fmt, Text;

(* "Kind" is an <<em>>enumeration<</em>> of kinds of extensions
   a file may have. Use "Kind.text_file" to refer to the "text_file" element 
   of the "Kind" enumeration. Enumerations provide symbolic names
   for a small range of ordinal values. *)

TYPE
  Kind = {module, interface, generic_module, generic_interface, 
          c_program, c_include, c_plus_plus_program, 
          acrobat_pdf, ps, text_file, 
          windows_executable, windows_batch, unix_shell, 
          html_markup_doc, compuserve_gif, jpeg};

(* "Description" is a constant array of text strings.
   While it could have been initialized with "ARRAY OF TEXT", 
   the index range "Kind" will keep this array synchronized
   with the "Kind" enumeration. *)

CONST
  Description = ARRAY Kind OF TEXT 
                  {"Module", "Interface", "Generic Module", "Generic Interface",
                   "C Program", "C Header", "C++ Program",
                   "Acrobat PDF", "PostScript", "Text File",
                   "Windows Executable", "Windows Batch", "Unix Shell",
                   "HTML Document", "GIF Image", "JPEG Image"};


(* "BasicExtensions" is another array of "TEXT" strings; this one
   describes extensions for the different kinds of files. *)

CONST
  BasicExtensions = 
    ARRAY Kind OF TEXT 
         {"m3", "i3", "mg", "ig", 
          "c", "h", "cpp", 
          "pdf", "ps", "txt", 
          "exe", "bat", "sh", 
          "html", "gif", "jpeg"};

(* Type "File" is a record composed of a file kind and the
   designated extension. We will use an array of "File"s to 
   store the database of all possible extensions. We need
   this array because some file kinds may have multiple
   extensions (e.g., "jpeg", and "jpg"). *)

TYPE
  File = RECORD
    kind: Kind;
    ext:  TEXT;
  END;

(* Extra extensions is a constant array of "File" records.
   It defines the extra extension characters that map to
   some of the basic file kinds. *)

CONST
  ExtraExtensions = 
    ARRAY OF File { File {Kind.text_file,            "text"},
                    File {Kind.html_markup_doc,      "htm"},
                    File {Kind.c_plus_plus_program,  "CPP"},
                    File {Kind.jpeg,                 "jpg"}};

(* "FileKind" is the index for the array of all extensions.
   This array, of course, includes the "BasicExtensions" and the 
   "ExtraExtenions". Note the use of "NUMBER" built-in operator.
   "NUMBER" will return the number of elements in a range or
   an array. *)

TYPE
  FileKind = [0 .. NUMBER(BasicExtensions)+NUMBER(ExtraExtensions)-1];

(* "Extensions" is an array of "File". This is the main data structure
   that keeps track of mappings from an extension to a file kind.
   See "Init", "Print", and "IsKnownExtension" for operations on
   this data structure. *)

VAR
  Extensions: ARRAY FileKind OF File;

PROCEDURE Init() = 
(* Initialize "Extensions" using "BasicExtensions" and "ExtraExtensions". *)
  BEGIN

   (* Loop through and install all basic extensions. Note the use of
      "FIRST" and "LAST" built-in operators. *)

    FOR i := FIRST(BasicExtensions) TO LAST(BasicExtensions) DO
      Extensions[ORD(i)] := File { kind := i, ext := BasicExtensions[i] };
    END;

   (* Assign "ExtraExtensions" to the rest of the array at once! *)

    SUBARRAY (Extensions, 
              NUMBER(BasicExtensions), NUMBER(ExtraExtensions)) := ExtraExtensions;
  END Init;

PROCEDURE Print(i: FileKind) =
(* Prints a description of a filekind to the screen. *)

  VAR
    file := Extensions[i];
  BEGIN
    IO.Put (Fmt.F("%10s %s\n", 
                    file.ext, Description[file.kind]));
  END Print;

PROCEDURE Dump() = 
(* Prints out the current state of the extension database. *)
  BEGIN
    FOR i := FIRST(Extensions) TO LAST(Extensions) DO
      Print(i);
    END;
  END Dump;

PROCEDURE IsKnownExtension(ext: TEXT; VAR kind: FileKind): BOOLEAN =
(* Checks whether an extension is in the database.
   If found, set "kind" to the index of the kind of file, and return "TRUE". 
   If not found, return "FALSE" to denote that "kind" is undefined. *)
  BEGIN
    FOR i := FIRST(Extensions) TO LAST(Extensions) DO
      IF Text.Equal (ext, Extensions[i].ext) THEN
        kind := i;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END IsKnownExtension;

PROCEDURE MainLoop() =
(* Loop: Read an extension from input, look it up in the database. *)
  BEGIN
    LOOP
      IO.Put ("Extension> ");

      WITH input = IO.GetLine() DO
(* The "WITH" statement creates a new scope where "input" is defined
   to be the result of "IO.GetLine()". *)
        IF Text.Equal(input, "quit") OR Text.Equal(input, "exit") THEN
          RETURN;
        ELSIF IsKnownExtension (input, kind) THEN
          Print (kind);
        ELSE
          IO.Put ("Extension \"" & input & "\" is unknown.\n");
        END;
      END;
    END;
  END MainLoop;    

VAR
  kind: FileKind;

BEGIN

  Init();
  
  IO.Put ("\n\n");
  IO.Put ("A sample extension mapping program.\n");
  IO.Put ("\n");

  IO.Put (".....Current Extension Mappings....\n");
  Dump();
  IO.Put ("\n");
  
  IO.Put ("....Type in extension to lookup....\n");
  IO.Put (".......Press Control-C to exit.....\n");
  IO.Put ("\n");
  MainLoop();
      
END ExtensionLookup.

