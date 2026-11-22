(* $Id: AL.i3,v 1.1 2007/06/21 01:16:32 mika Exp $ *)

INTERFACE AL;
IMPORT AtomList;

(* utility interface for error messages *)

TYPE T = AtomList.T;

PROCEDURE Format(t : T) : TEXT;

CONST Cons = AtomList.Cons;
      List1 = AtomList.List1;
      List2 = AtomList.List2;
      List3 = AtomList.List3;
      Length = AtomList.Length;
      Nth = AtomList.Nth;
      Append = AtomList.Append;
      AppendD = AtomList.AppendD;
      Reverse = AtomList.Reverse;
      ReverseD = AtomList.ReverseD;
      
END AL.
