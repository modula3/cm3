(* 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 *
 * This file is released under the same conditions as Pickle.m3. See COPYRIGHT.
 * 
 *)

UNSAFE MODULE PklTipeMap;

IMPORT Rd, Wr, RTPacking, RTTypeMap, Thread, ConvertPacking, RTHeap, IO, Fmt; 

PROCEDURE Read (v: ConvertPacking.ReadVisitor; r: REFANY; tc: TypeCode; 
                from: RTPacking.T; to: RTPacking.T; 
                READONLY shape: ARRAY OF INTEGER) RAISES 
        {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    nDim, fromEltPack, toEltPack: INTEGER;
    converter: ConvertPacking.T;
    addr := RTHeap.GetDataAdr(r);
    size: INTEGER := 1;
  BEGIN
    TRY
      converter := ConvertPacking.New(tc, from, to, nDim, 
                                      fromEltPack, toEltPack);

      (* converter.print(); *)
      IF nDim # NUMBER(shape) THEN
    	RAISE Error("Incorrect number of shape parameters");
      END;
      IF nDim > 0 THEN
    	(* Get the number of repetitions of the basic type in r *)
    	FOR i := FIRST(shape) TO LAST(shape) DO
    	  size := size * shape[i];
    	END;
  
    	(* SRC Modula-3 restriction: OpenArray elements are byte
    	   aligned, so we can depend on this!  *)
    	<* ASSERT fromEltPack MOD 8 = 0 AND toEltPack MOD 8 = 0 *>
    	fromEltPack := fromEltPack DIV 8;
    	toEltPack := toEltPack DIV 8;
  
(*
    	FOR i := 1 TO size DO
    	  WITH nextAddr = converter.convertRead(addr, v) DO
    	    <* ASSERT addr + toEltPack = nextAddr *>
    	    addr := nextAddr;
    	  END;
    	END;
*)
        WITH nextAddr = converter.convertRead(addr, v, size) DO
          <* ASSERT addr + toEltPack * size = nextAddr *>
        END;
      ELSE (* Not open array. *) 
(*
    	WITH nextAddr = converter.convertRead(addr, v) DO
*)
    	WITH nextAddr = converter.convertRead(addr, v, 1) DO
          WITH size = RTHeap.GetDataSize(r) DO
            IF size # nextAddr - addr THEN
              IO.Put("convertRead error! size (" & Fmt.Int(size) & 
                ") not equal to nextAddr - addr (" &
                Fmt.Int(nextAddr-addr) & ") in conversion:\n");
              converter.print();
              <* ASSERT size = nextAddr - addr *>
            END;
          END;
    	END;
      END;
    EXCEPT
    | ConvertPacking.Error(t) => 
      RAISE Error("ConvertPacking.convert Error: " & t);
    END;
  END Read;

TYPE
  ReadVisitor = RTTypeMap.Visitor OBJECT
      start: ADDRESS;
    OVERRIDES
      apply := VisitRead;
    END;

CONST
  KindStr = ARRAY RTTypeMap.Kind OF TEXT{
    "Ref", "UntracedRef", "Proc",   (* traced ref, untraced ref, procedure *)
    "Real", "Longreal", "Extended",  (* floating point value *)
    "Int_1", "Int_2", "Int_4", "Int_8",     (* 1, 2, 4, or 8 byte signed integer *)
    "Word_1", "Word_2", "Word_4", "Word_8", (* 1, 2, 4, or 8 byte unsigned integer *)
    "Int_Field", "Word_Field",          (* signed or unsigned bit field *)
    "Set"                             (* bit set *)
  };

PROCEDURE VisitRead(v: ReadVisitor; field: ADDRESS; kind: RTTypeMap.Kind) =
  BEGIN
    WITH offset = LOOPHOLE(field - v.start, INTEGER) DO
      IO.Put("offset: " & Fmt.Int(offset) & " = " & KindStr[kind] & "\n");
    END;    
  END VisitRead;

CONST
  RefFields = RTTypeMap.Mask { 
    RTTypeMap.Kind.Ref, RTTypeMap.Kind.UntracedRef, RTTypeMap.Kind.Proc,
    RTTypeMap.Kind.Real, RTTypeMap.Kind.Longreal, RTTypeMap.Kind.Extended,
    RTTypeMap.Kind.Int_1, RTTypeMap.Kind.Int_2, RTTypeMap.Kind.Int_4, 
    RTTypeMap.Kind.Int_8,
    RTTypeMap.Kind.Word_1, RTTypeMap.Kind.Word_2, RTTypeMap.Kind.Word_4, 
    RTTypeMap.Kind.Word_8,
    RTTypeMap.Kind.Int_Field, RTTypeMap.Kind.Word_Field,
    RTTypeMap.Kind.Set
  };

PROCEDURE Write (v: ConvertPacking.WriteVisitor; r: REFANY; tc: TypeCode; 
                 from: RTPacking.T; READONLY shape: ARRAY OF INTEGER; n: INTEGER)  
    RAISES { Error, Wr.Failure, Thread.Alerted } =
  VAR
    nDim, fromEltPack, toEltPack: INTEGER;
    converter: ConvertPacking.T;
    addr := RTHeap.GetDataAdr(r);
    size: INTEGER := 1;
  BEGIN
    TRY
      converter := ConvertPacking.New(tc, from, from, nDim, 
                                      fromEltPack, toEltPack);
      IF nDim # n THEN
    	RAISE Error("Incorrect number of shape parameters");
      END;
      IF nDim > 0 THEN
    	(* Get the number of repetitions of the basic type in r *)
    	FOR i := FIRST(shape) TO LAST(shape) DO
    	  size := size * shape[i];
    	END;
  
    	(* SRC Modula-3 restriction: OpenArray elements are byte
    	   aligned, so we can depend on this!  *)
    	<* ASSERT fromEltPack MOD 8 = 0 AND toEltPack MOD 8 = 0 *>
        (* The machine is the same!! *)
    	<* ASSERT fromEltPack = toEltPack *>
    	toEltPack := toEltPack DIV 8;
  
(*
    	FOR i := 1 TO size DO
    	  WITH nextAddr = converter.write(addr, v) DO
    	    <* ASSERT addr + toEltPack = nextAddr *>
    	    addr := nextAddr;
    	  END;
    	END;
*)
        WITH nextAddr = converter.write(addr, v, size) DO
          <* ASSERT addr + toEltPack*size = nextAddr *>
        END;
      ELSE
(*
    	WITH nextAddr = converter.write(addr, v) DO
*)
    	WITH nextAddr = converter.write(addr, v, 1) DO
          WITH size = RTHeap.GetDataSize(r) DO
            IF size # nextAddr - addr THEN
              IO.Put("converter.write error! size (" & Fmt.Int(size) & 
                ") not equal to nextAddr - addr (" &
                Fmt.Int(nextAddr-addr) & ") in conversion:\n");
              converter.print();
              <*FATAL ANY*> BEGIN
                RTTypeMap.WalkRef(r, RefFields, NEW(ReadVisitor, start := addr));
              END;
              <* ASSERT size = nextAddr - addr *>
            END;
            (* converter.print();*)
          END;
    	END;
      END;
    EXCEPT
    | ConvertPacking.Error(t) => 
      RAISE Error("ConvertPacking.write Error: " & t);
    END;

  END Write;

BEGIN
END PklTipeMap.
