(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Tobias Hoellerer (htobias)
 * Created On      : Fri Nov 10 17:37:04 EST 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Aug  9 13:54:38 1997
 * Update Count    : 20
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1.1.1  2001/12/02 13:15:54  wagner
 * Blair MacIntyre's sharedobjgen package
 *
 * Revision 1.3  1997/08/11 20:36:32  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

MODULE SOxDummyCode;

IMPORT SOxCodeUtils, SOxCoder, Formatter, ImportList, SOxCodeGenError,
       Type, AtomList, Atom, CodeForType; 

REVEAL T = SOxCoder.T BRANDED OBJECT 
OVERRIDES
  InitImports := initImports;
  Import := import;
  Head := head;
  Decls := decls;
  Main := main;
  Bottom := bottom;
END;

VAR
  extraImports := 
        ARRAY [1..1] OF Atom.T{Atom.FromText("SharedObj")};

PROCEDURE initImports(<*UNUSED*>self: T;
                      basename: TEXT; 
                      imports: ImportList.T) =
  BEGIN
    CodeForType.AugmentImportList(imports, extraImports);
  END initImports;

PROCEDURE import(<*UNUSED*>self: T;
                type: Type.Object;  
                methods: ImportList.MethodList;
                umethods: AtomList.T;
                imports: ImportList.T) =
  BEGIN
    CodeForType.ImportLst(type, imports, methods, umethods);
  END import;

PROCEDURE head(self: T;
               wr: Formatter.T; 
               fname: TEXT; 
               basename: TEXT; 
               imports: ImportList.T) =
  BEGIN
    SOxCodeUtils.HeaderComment(wr, fname);
  END head;

PROCEDURE decls(self: T;
                wr: Formatter.T; 
                typeID: Type.Qid;  
                stypeID: Type.Qid;  
                implName: TEXT; 
                methods: ImportList.MethodList;
                umethods: AtomList.T) RAISES {SOxCodeGenError.E} =
  BEGIN
  END decls;

PROCEDURE main(self: T;
               wr: Formatter.T; 
               typeID: Type.Qid;  
               type: Type.Object;  
               stypeID: Type.Qid;  
               implName: TEXT; 
               methods: ImportList.MethodList;
               umethods: AtomList.T) RAISES {SOxCodeGenError.E} =
  BEGIN
  END main;

PROCEDURE bottom(self: T;
                 wr: Formatter.T; 
                 fname: TEXT) =
  BEGIN
  END bottom;

BEGIN
END SOxDummyCode.







