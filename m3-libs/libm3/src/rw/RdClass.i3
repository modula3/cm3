(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Jun 18 16:18:48 PDT 1993 by wobber         *)
(*      modified on Tue Jun 15 10:07:07 1993 by gnelson        *)
(*      modified on Fri May 21 09:50:56 PDT 1993 by swart      *)
(*      modified on Mon Apr 26 17:22:23 PDT 1993 by mcjones    *)
(*      modified on Wed Nov  6 10:45:09 PST 1991 by kalsow     *)
(*      modified on Fri Sep 28 23:12:34 1990 by muller         *)


(* The RdClass interface is analogous to the WrClass interface. It
reveals that every reader contains a buffer of characters together
with methods for managing the buffer.  New reader classes are created
by importing RdClass (to gain access to the buffer and the methods)
and then defining a subclass of Rd.T whose methods provide the new
class's behavior.  The opaque type Private hides irrelevant details of
the class-independent code. *)
   
INTERFACE RdClass;
IMPORT Rd;
FROM Thread IMPORT Alerted;
FROM Rd IMPORT Failure;

TYPE
  Private <: ROOT;
  SeekResult = {Ready, WouldBlock, Eof};

REVEAL
  Rd.T =
    Private BRANDED OBJECT
      buff         : REF ARRAY OF CHAR;
      st           : CARDINAL;           (* index into buff *)
      lo, hi, cur  : CARDINAL            := 0; (* indexes into src(rd) *)
      closed: BOOLEAN := TRUE;   (* init method of the subtype should set
                                    this to FALSE *)
      seekable, intermittent: BOOLEAN;
    METHODS
      seek   (n: CARDINAL; dontBlock: BOOLEAN): SeekResult
                         RAISES {Failure, Alerted};
      getSub (VAR a: ARRAY OF CHAR): CARDINAL
                         RAISES {Failure, Alerted} := GetSubDefault;
      length (): INTEGER RAISES {Failure, Alerted} := LengthDefault;
      close  () RAISES {Failure, Alerted}          := CloseDefault;
    END;


(* Let rd be a reader, abstractly given by len(rd), src(rd), cur(rd),
avail(rd), closed(rd), seekable(rd), and intermittent(rd).  The data
fields cur, closed, seekable, and intermittent in the object represent
the corresponding abstract attributes of rd.  The buff, st, lo, and hi
fields represent a buffer that contains part of src(rd), the rest of
which is represented in some class-specific way. 

More precisely, we say that the state of the representation is valid
if conditions V1 through V4 hold:

V1. the characters of buff starting with st accurately reflect src. 
    That is,  for all i in [rd.lo .. rd.hi-1],
           
	rd.buff[rd.st + i - rd.lo] = src(rd)[i]

V2. the cur field is in or just past the end of the buffer, that is:

        rd.lo <= rd.cur <= rd.hi

V3. the reader does not claim to be both intermittent and seekable:

	NOT (rd.intermittent AND rd.seekable)

It is possible that buff = NIL in a valid state, since the range of
i's in V1 may be empty; for example, in case lo = hi.

V4. if closed(rd) then rd.buff = NIL AND rd.lo = rd.hi

If rd is valid and cur(rd) is less than rd.hi, we say the reader
is ready.  More precisely, rd is ready if:

   NOT rd.closed  AND  rd.buff # NIL  AND  rd.lo <= rd.cur < rd.hi

If the state is ready, then Rd.GetChar can be implemented by fetching
from the buffer.  Together V1, V2, and V4 imply that if rd.cur # rd.hi
then rd.buff # NIL and NOT rd.closed.  Therefore a valid reader is ready
if "rd.cur # rd.hi".

The class-independent code modifies rd.cur, but no other variables
revealed in this interface (except that "Rd.Close" modifies "rd.lo" and
"rd.cur" and sets "rd.buff" to NIL in order to maintain invariant V4).  The
class-independent code locks the reader before calling any methods.

Here are the specifications for the methods:

The basic purpose of the seek method is to make the reader ready.  To
seek to a position n, the class-independent code checks whether the reader
would be ready with rd.cur = n and if so, simply sets rd.cur to n.
If not, it calls rd.seek supplying the position n as argument.
As in the case of writers, the seek method can be called even for an
unseekable reader in the special case of advancing to the next buffer.
There is a wrinkle to support the implementation of CharsReady.  If rd
is ready, the class-independent code can handle the call to
CharsReady(rd) without calling any methods (since there is at least
one character ready in the buffer), but if rd.cur = rd.hi, then the
class independent code needs to find out from the class implementation
whether any characters are ready in the next buffer.  Using the seek
method to advance to the next buffer won't do, since this could block,
and CharsReady isn't supposed to block.  Therefore, the seek method
takes a boolean argument saying whether blocking is allowed. If
blocking is forbidden and the next buffer isn't ready, the method
returns the special value WouldBlock; this allows the
class-independent code to return zero from CharsReady.  The "dontBlock"
boolean should be "TRUE" only if the seek method is being used to advance
to the next buffer.

More precisely, given a valid state where

     (n # rd.hi) => rd.seekable
AND  (dontBlock => n = rd.hi)

the call res := rd.seek(n, dontBlock) establishes a valid state.
Furthermore, if res = Ready then rd is ready and rd.cur = n;
while if res = Eof, then rd.cur = len(rd); and finally if res = WouldBlock
then dontBlock was TRUE and avail(rd) = cur(rd).

The getSub method is used to implement Rd.GetSub and is
called with the reader lock held.  Efficient implementations override
this method to avoid unnecessary copying by reading directly
from the reader source, bypassing the reader buffer.  The default
implementation is correct for any class, but always copies through
the reader buffer.

The length method returns the length of a non-intermittent reader.
That is: Given a valid state in which rd.intermittent is FALSE, the
call rd.length() returns len(rd) without changing the state of rd.  An
intermittent reader may return the length if it is known, or -1.

The close method releases all resources associated with rd.  The exact
meaning of this is class-specific.  "Rd.Close" sets the "buff" field
to "NIL", so the method need not do this.  When the method is 
called the state will be valid; validity is not required when the 
method returns (since after it returns, the class-independent code 
will set the closed bit in the reader, which makes the rest of 
the state irrelevant).

The remainder of the interface is similar to the corresponding part 
of the WrClass interface: *)


PROCEDURE Lock(rd: Rd.T) RAISES {};
(* The reader rd must be unlocked; lock it and make its state valid. *)

PROCEDURE Unlock(rd: Rd.T) RAISES {};
(* The reader rd must be locked and valid; unlock it and restore the
private invariant of the reader implementation. *)

PROCEDURE GetSubDefault(rd: Rd.T; VAR (*OUT*) str: ARRAY OF CHAR): CARDINAL
  RAISES {Failure, Alerted};
(* Implement "getSub" by copying from the buffer, calling the "seek"
   method as necessary.  Clients can override this in order to
   achieve greater efficiency; for example, by copying directly
   from the source of the reader into "str". *)

PROCEDURE LengthDefault(rd: Rd.T): INTEGER RAISES {Failure, Alerted};
(* The procedure LengthDefault causes a checked runtime error; this
represents an error in the (non-intermittent) class implementation. *)

PROCEDURE CloseDefault(rd: Rd.T) RAISES {Failure, Alerted};
(* The procedure CloseDefault is a no-op. *)

END RdClass.

