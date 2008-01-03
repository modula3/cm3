(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Sun Aug  8 15:18:14 PDT 1993 by heydon                   *)

INTERFACE Packet;

TYPE
  Array = REF ARRAY OF T;
  T <: TPub; TPub = ROOT BRANDED OBJECT
    id: CARDINAL;
  METHODS
    init(src, finDest: CARDINAL): T;
    curr(): CARDINAL;
    dest(): CARDINAL;
    done(): BOOLEAN;
    moveTo(to: CARDINAL);
  END;

  (* If "p" is a "Packet.T", then "p.id" is the unique identifier of the
     packet. The call "p.init(src, finDest)" initializes the packet at node
     "src", with final destination "finDest".

     "p.dest()" is the current destination of the packet. By default, it is
     the destination "dest" passed in the most recent call to "init". However,
     subtypes may override this method to implement the notion of one or more
     intermediate destination nodes.

     The call "p.done()" is true iff the packet has reached its final
     destination, that is, if its current location is the "dest" passed in the
     most recent call to "init".

     The call "p.moveTo(to)" specifies that "to" is packet "p"'s new current
     node. *)

EXCEPTION BadPkts(TEXT);

END Packet.
