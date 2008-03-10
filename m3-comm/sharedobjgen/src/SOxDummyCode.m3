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
 * $Source: /opt/cvs/cm3/m3-comm/sharedobjgen/src/SOxDummyCode.m3,v $
 * $Date: 2008-03-10 13:34:32 $
 * $Author: hosking $
 * $Revision: 1.4 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2008-01-13 03:57:47  jkrell
 * fix some but not all of the warnings here
 * it would be nice if UNUSED could be turned off for an entire file
 * it is not a particularly useful warning, as it falls out of certain
 * common designs and I turn it off all the time in my C and C++ code..
 *
 * Revision 1.2  2001-12-03 17:23:37  wagner
 * add copyright notes and overrides
 *
 * added: sharedobjgen/COPYRIGHT
 * added: sharedobjgen/COPYRIGHT-COLUMBIA
 * added: sharedobjgen/src/COPYRIGHT-COLUMBIA
 * added: sharedobjgen/src/m3overrides
 * modified: sharedobjgen/src/SOxCodeFiles.i3
 * modified: sharedobjgen/src/SOxCodeFiles.m3
 * modified: sharedobjgen/src/SOxCodeGenError.i3
 * modified: sharedobjgen/src/SOxCodeGenError.m3
 * modified: sharedobjgen/src/SOxCodeUtils.i3
 * modified: sharedobjgen/src/SOxCodeUtils.m3
 * modified: sharedobjgen/src/SOxCoder.i3
 * modified: sharedobjgen/src/SOxDummyCode.i3
 * modified: sharedobjgen/src/SOxDummyCode.m3
 * modified: sharedobjgen/src/SOxIntfCBCode.i3
 * modified: sharedobjgen/src/SOxIntfCBCode.m3
 * modified: sharedobjgen/src/SOxIntfCBProxyCode.i3
 * modified: sharedobjgen/src/SOxIntfCBProxyCode.m3
 * modified: sharedobjgen/src/SOxIntfPklCode.i3
 * modified: sharedobjgen/src/SOxIntfPklCode.m3
 * modified: sharedobjgen/src/SOxIntfProxyCode.i3
 * modified: sharedobjgen/src/SOxIntfProxyCode.m3
 * modified: sharedobjgen/src/SOxModuleCBCode.i3
 * modified: sharedobjgen/src/SOxModuleCBCode.m3
 * modified: sharedobjgen/src/SOxModuleProxyCode.i3
 * modified: sharedobjgen/src/SOxModuleProxyCode.m3
 * modified: sharedobjgen/src/SOxModuleSOCode.i3
 * modified: sharedobjgen/src/SOxModuleSOCode.m3
 * modified: sharedobjgen/src/StubGenTool.i3
 * modified: sharedobjgen/src/StubGenTool.m3
 *
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
                      <*UNUSED*>basename: TEXT; 
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

PROCEDURE head(<*UNUSED*>self: T;
               wr: Formatter.T; 
               fname: TEXT; 
               <*UNUSED*>basename: TEXT; 
               <*UNUSED*>imports: ImportList.T) =
  BEGIN
    SOxCodeUtils.HeaderComment(wr, fname);
  END head;

PROCEDURE decls(<*UNUSED*>self: T;
                <*UNUSED*>wr: Formatter.T; 
                <*UNUSED*>typeID: Type.Qid;  
                <*UNUSED*>stypeID: Type.Qid;  
                <*UNUSED*>implName: TEXT; 
                <*UNUSED*>methods: ImportList.MethodList;
                <*UNUSED*>umethods: AtomList.T) RAISES <*NOWARN*> {SOxCodeGenError.E} =
  BEGIN
  END decls;

PROCEDURE main(<*UNUSED*>self: T;
               <*UNUSED*>wr: Formatter.T; 
               <*UNUSED*>typeID: Type.Qid;  
               <*UNUSED*>type: Type.Object;  
               <*UNUSED*>stypeID: Type.Qid;  
               <*UNUSED*>implName: TEXT; 
               <*UNUSED*>methods: ImportList.MethodList;
               <*UNUSED*>umethods: AtomList.T) RAISES <*NOWARN*> {SOxCodeGenError.E} =
  BEGIN
  END main;

PROCEDURE bottom(<*UNUSED*>self: T;
                 <*UNUSED*>wr: Formatter.T; 
                 <*UNUSED*>fname: TEXT) =
  BEGIN
  END bottom;

BEGIN
END SOxDummyCode.
