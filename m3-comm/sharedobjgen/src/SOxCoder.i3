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
 * Last Modified On: Sat Aug  9 13:53:17 1997
 * Update Count    : 19
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobjgen/src/SOxCoder.i3,v $
 * $Date: 2001-12-03 17:23:37 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 13:15:54  wagner
 * Blair MacIntyre's sharedobjgen package
 *
 * Revision 1.3  1997/08/11 20:36:31  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

INTERFACE SOxCoder;

IMPORT Formatter, ImportList, SOxCodeGenError, AtomList, Type;

TYPE 
  T = OBJECT
    basename : TEXT;
    fbasename : TEXT;
  METHODS
    InitImports(basename: TEXT; 
                imports: ImportList.T);
    Import(typeID: Type.Object;  
           methods: ImportList.MethodList;
           umethods: AtomList.T;
           imports: ImportList.T);
    Head(modWr: Formatter.T; 
         fname: TEXT;
         basename: TEXT; 
         imports: ImportList.T);
    Decls(modWr: Formatter.T; 
          typeID: Type.Qid;  
          stypeID: Type.Qid;  
          implName: TEXT; 
          methods: ImportList.MethodList;
          umethods: AtomList.T) RAISES {SOxCodeGenError.E};
    Main(modWr: Formatter.T; 
         typeID: Type.Qid;  
         type: Type.Object;  
         stypeID: Type.Qid;  
         implName: TEXT; 
         methods: ImportList.MethodList;
         umethods: AtomList.T) RAISES {SOxCodeGenError.E};
    Bottom(modWr: Formatter.T; 
           fname: TEXT);
  END;

(*
  t: Type.Object; 
  intWr: Formatter.T; 
  typeName: Atom.T;
  objName: Type.Qid; 
  methods: StubCode.MethodList;
  lastNewMethod: INTEGER;
  imports: AtomRefTbl.T);
*)


END SOxCoder.
