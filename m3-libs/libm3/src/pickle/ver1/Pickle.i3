(* Copyright 1992 Digital Equipment Corporation             *)
(* Distributed only by permission.                          *)
(* Last modified on Wed Feb 10 16:25:53 1993 by birrell *)
(*      modified on Tue Dec  8 11:32:52 1992 by gnelson *)

(* A {\it pickle} is a representation of a Modula-3 value as 
   a stream of bytes.  Writing a value as a pickle and 
   then reading it back produces a value ``equivalent'' to the 
   original value, in the sense that the value's non-ref 
   fields have the same contents and that their "REF" and 
   "OBJECT" fields are "=" in the same places.  In other words, 
   pickles preserve value, shape and sharing.  You can write 
   pickles for values that have cyclic references (such as 
   doubly-linked lists), or that are arbitrary graph structures. 

   Pickles are used both to communicate values between two
   programs running at the same time and to store a value and 
   reuse it in a program running later.

   Pickles preserve ordinal values (integers and enumerations)
   exactly,  automatically performing any byte-swapping 
   that may be required. An exception is raised if a pickled 
   ordinal value is too large to be represented in a program 
   reading the pickle.  

   Pickles preserve floating point values as well as possible
   considering that the two machines may have different
   floating point representations.

   Pickles do not preserve untraced references.  Any untraced
   references will turn into "NIL" if they are pickled,
   regardless of their value.

   Pickles preserve the names and signatures of procedures,
   but the code for the procedures can be different in
   the program writing the pickle and the program
   reading the pickle.  Object methods are treated
   the same as procedures.

   Sharing is preserved within pickles, but not between pickles.

   Pickles are written and read by a single-pass 
   algorithm that uses modest auxiliary tables and does not
   require a seekable stream. 
   
   For example, if you compile and run the following program:

| MODULE P1 EXPORTS Main;  IMPORT Pickle, IO;
|   TYPE T = REF RECORD val: INTEGER END;
| BEGIN
|   Pickle.Write(NEW(T, val := 6), IO.OpenWrite("A.pickle"))
| END P1.

   and then in the same directory, you compile and run the
   following program:

| MODULE P2 EXPORTS Main; IMPORT Pickle, IO;
|   TYPE U = REF RECORD val: INTEGER END;
|   VAR v: U := Pickle.Read(IO.OpenRead("A.pickle"));
| BEGIN
|   IO.Put(Fmt.Int(v.val) & "\n")
| END P2.

   then "P2" will read the pickle written by "P1" and will
   print "6".  Notice that the type of the pickled reference
   was named differently in "P1" and "P2"; but this doesn't
   present any problem, since Modula-3
   uses structural equivalence.

   If you pickle a "BRANDED REF INTEGER", there is no
   guarantee that you will be able to read it back as
   a "BRANDED REF INTEGER", since the implicit brand can
   vary from program to program.  Therefore, you should
   use explicit brands in types that will be pickled.
*)

INTERFACE Pickle;

IMPORT Rd, Thread, Wr;

EXCEPTION Error(TEXT);

PROCEDURE Write(wr: Wr.T; ref: REFANY)
    RAISES {Error, Wr.Failure, Thread.Alerted};
(* Trace the data structure reachable via traced references 
   from "ref", convert it into a pickle, and write the pickle to 
   "wr", starting at the current position of "wr" and leaving 
   "wr" positioned after the last byte written. *)

PROCEDURE Read(rd: Rd.T): REFANY
    RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Read a pickle from "rd", reconstruct a copy of the pickled
   data structure, and return it.  The bytes to be read start at
   "rd"'s current position; "rd" is left positioned after the
   last byte consumed. *)

(* Most people should go no further.  The remainder of the 
   interface allows you to customize the pickling and unpickling
   of selected types.  There are two ways of doing this: you
   can override the methods of objects called {\it pickle writers}
   and {\it pickle readers}, or you can register {\it specials}
   for particular types.  Several examples are presented
   after the end of the interface.

   \paragraph{Pickle writers and readers.}
   A "Pickle.Writer" is an object whose "write" method writes
   pickles to the writer stored in its "wr" data field.  
   You can override the "write" method to produce special
   effects. *)
   
TYPE
  TypeCode = INTEGER; 
(* Used to represent types of allocated objects. A "TypeCode"
   value is valid within a single execution of a single program.
   There are methods in "Writer" and "Reader" objects for writing
   and reading program-independent representations of a TypeCode.
   *)

TYPE
  Writer <: WriterPublic;
  WriterPublic = OBJECT
      wr: Wr.T;
    METHODS
      write(r: REFANY) RAISES
        {Error, Wr.Failure, Thread.Alerted};
      writeType(tc: TypeCode) RAISES
        {Wr.Failure, Thread.Alerted};
      writeInt(i: INTEGER) RAISES
        {Wr.Failure, Thread.Alerted};
    END;

(* If "w" is a "Pickle.Writer", then "w.write(r)"
   traces the data structure reachable via traced references 
   from "r", converts it into a pickle, and writes the pickle to 
   "w.wr", starting at the current position of "w.wr" and 
   leaving "w.wr" positioned after the last byte written.

   The default method "w.write(r)" behaves as follows:

\begin{itemize}
\item If "r=NIL", write a representation of "NIL" on "w.wr".

\item Otherwise if "r" has been previously written to the 
      current pickle, write the index of "r" in this pickle 
      on "w.wr".

\item Otherwise, find the "Special" (as defined below) for 
      the closest supertype of "r"'s allocated type for 
      which a special has been registered. Let "sp" be this 
      special (note that there always is such a special, since
      the implementation registers a special for "REFANY"). Call
      "writeType(sp.sc)", then write a representation of
      "TYPECODE(r)", then invoke "sp.write(r, w)".   
\end{itemize}

   The call "w.writeType(tc)" writes bytes on w.wr to represent
   the type "tc" in a program-independent fashion. These bytes
   are a fingerprint: fingerprints have the property that if
   two Modula-3 types are equivalent, then their fingerprints
   are equal; and with extremely hight probability, if their
   fingerprints are equal then they are equivalent. Note that
   the implementation of this method optimizes so that if a type
   is written to a pickle many times, occurences after the first
   typically occupy only one byte.

   The call "w.writeInt(i)" writes bytes on "w.wr" to represent
   the integer "i" in a platform-independent fashion. In
   particular, the "readInt" method of a subsequent "Pickle.Reader"
   can read the integer correctly, even if executed on a platform
   with different byte order or integer size (if possible).

   A "Pickle.Writer" may be serially re-used to pickle multiple
   objects.

   The call "Pickle.Write(wr, ref)" is equivalent to (but more 
   efficient than) the following:
   
| NEW(Pickle.Writer, wr := wr).write(ref)

   A "Pickle.Reader" is an object whose "read" method reads
   pickles from the reader stored in its "rd" data field.  
   You can override the "read" method to produce special
   effects. *)
   
TYPE
  Reader <: ReaderPublic;
  RefID = INTEGER;
  ReaderPublic = OBJECT
      rd: Rd.T;
    METHODS
      read(): REFANY RAISES
        {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
      readType(): TypeCode RAISES
        {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
      readInt(): INTEGER RAISES
        {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
      noteRef(ref: REFANY; id: RefID);
    END;

(* If "r" is a "Pickle.Reader", then "r.read()"
   reads a pickle from "r.rd", reconstructs a copy of 
   the pickled data structure, and returns it.  The bytes 
   to be read start at "r.rd"'s current position; 
   "r.rd" is left positioned after the last byte consumed.   

   The default method "r.read(r)" behaves as follows:

\begin{itemize}
\item If "r.rd" contains a representation of "NIL", consume those 
      bytes and return "NIL".

\item Otherwise, if "r.rd" contains the index of a previously 
      read value in this pickle, consume those bytes and return
      that value.

\item Otherwise compute "sc = readType()", and proceed as follows.
      Find the registered special 
      "sp" (as defined below) whose "sp.sc" field equals "sc".  
      Read the representation of a type code "ac" from "r.rd".  
      Let "id" be the index that the result of this method will 
      have in the pickle. Invoke "sp.read(ac, r, id)", call 
      "r.noteRef(result, id)", and return the result.   Note that
      in the default case "sp" will be "RootSpecial()", defined
      below. 
\end{itemize}

   In order to implement the second case above, the "Pickle.Reader"
   maintains a table mapping indexes to references.  In simple cases
   this table can be maintained entirely by the "r.read" method,
   which places the result of the "sp.read" invocation into the
   table. But if the value being unpickled will contain an occurence
   of its own reference (the second case above), the reference must
   be placed in the table before unpickling that occurence. To do
   so, the special's "read" method must call "r.noteRef" before any
   such occurrence.  More exactly, a special's "read" method must
   call "r.noteRef" before any occurrence of the ref that the "read"
   method will return, giving "noteRef" the reference and the
   "refID" that was a parameter of the "read" method. If there is no
   such occurrence, the special need not (but may) call "r.noteRef".
   It is a checked runtime error to call "r.noteRef" with a value other
   than the value that will be returned by the special's "read" method;
   it is OK to call "r.noteRef" multiple times with the same value.

   The call "r.readType" reads bytes from "r.rd" that represent
   a type, as written by the "writeType" method of a "Writer",
   and converts them into a typecode valid in the current
   execution of the current program. If there is no such type,
   or if the bytes couldn't correspond to ones written by the
   "writeType" method, raises "Pickle.Error".

   The call "r.readInt" reads bytes from "r.rd" that represent an
   integer, as written by a writer's "writeInt" method, and
   converts them into an integer, taking into consideration
   reperesentation differences such as byte order and integer
   length. It raises "Pickle.Error" if the integer cannot be represented
   on this machine.

   A "Pickle.Reader" may be serially re-used to unpickle multiple
   objects.

   The call "Read(rd)"  is equivalent to (but more efficient than)
   the following:

| RETURN NEW(Reader, rd := rd).read();

\paragraph{Specials.}
   Specials provide for customized pickling of specified data 
   types on every call of "read" or "write" in this process.  A
   client can achieve this effect for individual calls of "read"
   or "write" by using a sub-class of "Pickle.Reader" or
   "Pickle.Writer", and checking for that sub-class (using
   "TYPECASE") inside the special.  The implementation of this
   interface registers a special for the type "REFANY"; this is
   special is the value of a call "NEW(Special)", i.e. its
   methods are the default methods of the type "Special".   There
   are three constraints on the methods of a special:

\begin{itemize}
\item  the methods must leave the "Rd.T" or "Wr.T" positioned
       after the last byte read or written;

\item  the "read" method must consume the number of bytes written
       by the "write" method;

\item  the "read" method must produce a value equivalent to the
       one that was given to the "write" method.
\end{itemize}

  If these rules are violated, the result could be either a
  checked runtime error or an invalid result from reading a
  pickle.

  There are many ways to program a special.  For example, 
  the "write" method could modify the value and then call 
  the root special.  Or the "write" method could create a related
  value and call "writer.write" or "Special.write". Or it
  could write some data fields individually and call
  "writer.write" for selected sub-values of its value.  Or it
  could use mixtures of these techniques. *)

TYPE
  SpecialPublic = OBJECT
      sc: TypeCode; 
    METHODS
      write(ref: REFANY; writer: Writer)
        RAISES {Error, Wr.Failure, Thread.Alerted};
      read(reader: Reader; id: RefID): REFANY
        RAISES {Error, Rd.EndOfFile, Rd.Failure, 
          Thread.Alerted};
    END;
  Special <: SpecialPublic;

(* The special's "write" method should write a representation 
   of "ref"'s contents on "wr", in a manner analogous to
   the default "Special.write" method. The value "ref" is 
   not "NIL" and its allocated type is a sub-type of "sc".

   The default "write" method calls
   "writer.writeType(TYPECODE(ref))", then writes on "writer.wr" the
   bytes of "ref"'s untraced data fields, then calls
   "writer.write(x)" for each traced value "x" directly contained
   in "ref".

   The special's "read" method must return an appropriate value
   read from "rd". If the value might involve a reference to
   itself, the special must pass the value and "id" to "r.noteRef"
   before the first such occurrence of the value. In other
   situations the special need not (but may) call "r.noteRef".
   See the earlier discussion of the "r.read" method.

   The result of the "read" method must be a value acceptable in
   the context where the method is being invoked. For example, if
   the method is being invoked to read the contents of a field
   of a record, the result must be assignable to the declared type
   of the field.  It is always correct to return a value of the
   same type as the value that was written when the pickle was
   created, but a sub-type might be acceptable too.

   The default "read" method reads a typecode "tc" by calling
   "reader.readType()", allocates a value whose type-code is "tc",
   passes the allocated value to the reader's "noteRef" method,
   reads the bytes of its untraced data fields from "reader.rd", 
   then for each directly contained traced reference 
   calls "reader.read()" and assigns the result into the 
   appropriate field of the allocated value; finally it returns 
   the value.
   
   Note that the special's methods can use the "writeType" and
   "readType" methods of the "Writer" or "Reader" to record
   relevant type information. For example they could record the
   allocated type of the value being pickled in the same way as
   the default methods do, or they could record more extensive
   type information if available, such as the entire type
   ancestry of the value's type. *)

PROCEDURE RegisterSpecial(sp: Special);
(* Register "sp" as the special for pickling and unpickling 
   objects having type code "sp.sc". *)
   
(* After you call "RegisterSpecial(sp)", the special "sp" will be
   called from a "Pickle.Writer"'s "write" method, or a
   "Pickle.Reader"'s "read" method, to pickle or unpickle objects
   whose type is "sp.sc", or whose type is a sub-type of "sp.sc"
   and has no closer registered special.  It is a checked runtime
   error to register a special "sp" when there is already a
   special registered with the same "sp.sc".  There is always a
   special registered for "REFANY" that special's methods are
   the default methods of the type "Special". *)

END Pickle.

(* \paragraph{Examples.}  For example, suppose you
are writing a pickle that contains many references of
type "TEXT", and you want to modify the pickling process
so that any texts that are "Text.Equal" will share storage.
(By default, texts in the pickle will share storage if and only if
they shared storage in the original data structure.) Of course this 
modification will mean that the unpickled value will have more sharing than 
the original value. A simple way to achieve this is to build the following
subtype of "Pickle.Writer":

| TYPE WriterP = Pickle.Writer OBJECT
|   tbl: TxtTxtTbl.T
| METHODS
|   init(): WriterP := InitP
| OVERRIDES
|   write := WriteP
| END;
|
| PROCEDURE WriteP(self: WriterP; r: REFANY)
|   RAISES {Error, Wr.Failure, Thread.Alerted} =
|   VAR txt: TEXT;
|   BEGIN
|     IF ISTYPE(r, TEXT) THEN
|       IF self.tbl.get(r, txt) THEN 
|         r := txt
|       ELSE
|         EVAL self.tbl.put(r, r)
|       END
|     END;
|     Pickle.Writer.write(self, r)
|   END WriteP;
|
| PROCEDURE InitP(self: WriterP): WriterP =
|   BEGIN
|     self.tbl := NEW(TxtTxtTbl.T).init()
|     RETURN self
|   END InitP;

The "WriterP" object keeps a table with an entry for
every text that has been encountered in the pickle.
When the method "WriteP" encounters
a reference "r" of type "TEXT", it checks if the table contains
a text "txt" that is "Text.Equal" to "r".  (A "TxtTxtTbl.T"
considers two texts to be the same
key if they are "Text.Equal".)  If so, "txt" is pickled
instead of "r"; if not, "r" is entered into the table
so that if any text is later encountered that is
"Text.Equal" to "r", then "r" will be pickled in
its place.  In either case, the subtype method "WriteP"
delegates the actual work of writing the bytes into
the pickle to its supertype method
"Pickle.Writer.write".  

Given the program above, you can pickle a data
structure "ref" to the writer "wr", identifying texts 
that have the same contents, with:

|  NEW(WriterP, wr := wr).init()(ref)

For this example, no special processing is necessary when 
reading; you just use an ordinary "Pickle.Reader", or
a normal call to "Pickle.Read". 

Next we consider a simple example of the use of specials.  
Suppose that you have a type defined by

| TYPE T = REF RECORD ref: REFANY; tc: CARDINAL END

in which the "tc" field represents a Modula-3 typecode.
Since typecodes are address-space specific, it is necessary
to translate the "tc" field so that it represents the same
Modula-3 type in the address space reading the pickle as
it did in the address space that wrote the pickle.  This
is achieved by using the procedures "RTTypeFP.ToFingerPrint"
and "RTTypeFP.FromFingerprint", which translate between
typecodes and type fingerprints.  A type fingerprint is
a 64-bit hash value that with high probability identifies
a Modula-3 type in an address-space independent way.
A fingerprint is represented as an "ARRAY [0..7] OF [0..255]". 

Here is one way to register specials for type "T":

| TYPE TSpecial = Pickle.Special OBJECT
|   OVERRIDES
|     write := WriteT;
|     read := ReadT
|   END;

| PROCEDURE WriteT(
|     self: TSpecial; 
|     ref: REFANY; 
|     wr: Pickle.Writer)
|   RAISES {Error, Wr.Failure, Thread.Alerted} =
|   VAR 
|     t: T := ref; 
|     fp := RTTypeFP.ToFingerPrint(t.tc); 
|   BEGIN
|     FOR i := 0 TO 7 DO 
|       Wr.PutChar(wr.wr, VAL(fp[i], CHAR)) 
|     END;
|     wr.write(t.ref)
|   END WriteT;

| PROCEDURE ReadT(
|     ac: INTEGER; 
|     rd: Pickle.Reader; 
|     id: RefID): REFANY
|   RAISES {Error, Rd.EndOfFile, Rd.Failure, 
|           Thread.Alerted} =
|   VAR fp: ARRAY [0..7] OF [0..255]; res: T; BEGIN
|     FOR i := 0 TO 7 DO 
|       fp[i] := ORD(Rd.GetChar(rd.rd)) 
|     END;
|     res := NEW(t, tc := RTTypeFP.FromFingerprint(fp));
|     rd.noteRef(res, id);
|     res.ref := rd.read();
|     RETURN res
|   END ReadT;

| ...

| Pickle.RegisterSpecial(
|   NEW(TSpecial, tc := TYPECODE(T)))

It is important to understand the call to "rd.noteRef" made by
the special's read method.  The effect of the call is to
make an entry in the pickle reader's table recording that "res"
is the reference associated with "id".  This is essential because
the recursive call to "rd.read" might encounter additional
instances of "id", if some value "t" of type "T" is reachable
from its own "t.ref" field.

As a final example, suppose that "T" is an
object instead of a "REF RECORD":

| TYPE T = OBJECT ref: REFANY; tc: CARDINAL END.

The solution above still works for objects whose
allocated type is "T", but it doesn't work for objects
whose allocated type is a subtype of "T" that
has additional fields.  Here are new methods for
the special that automatically handle subtypes
(assuming that any additional fields do not themselves require 
non-standard translation):

| PROCEDURE WriteT(
|     self: TSpecial; 
|     ref: REFANY; 
|     wr: Pickle.Writer)
|   RAISES {Error, Wr.Failure, Thread.Alerted} =
|   VAR 
|     t: T := ref; 
|     fp := RTTypeFP.ToFingerPrint(t.tc); 
|   BEGIN
|     FOR i := 0 TO 7 DO 
|       Wr.PutChar(wr.wr, VAL(fp[i], CHAR)) 
|     END;
|     Special.write(t, wr)
|   END WriteT;

| PROCEDURE ReadT(
|     ac: INTEGER; 
|     rd: Pickle.Reader; 
|     id: RefID): REFANY
|   RAISES {Error, Rd.EndOfFile, Rd.Failure, 
|           Thread.Alerted} =
|   VAR 
|     fp: ARRAY [0..7] OF [0..255]; 
|     res: T; 
|   BEGIN
|     FOR i := 0 TO 7 DO 
|       fp[i] := ORD(Rd.GetChar(rd.rd)) 
|     END;
|     res := Special.Read(ac, rd, id);
|     res.tc := RTTypeFP.FromFingerprint(fp);
|     RETURN res
|   END ReadT;

In this case the special uses the root special to transmit the whole
object in the usual way.  The write special also sends the
fingerprint, which the read special uses to fix up the typecode. 
Notice that with this strategy, the call to "rd.noteRef" is not
required. *)
