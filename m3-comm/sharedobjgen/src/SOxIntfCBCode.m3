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
 * Last Modified On: Sat Aug  9 13:54:36 1997
 * Update Count    : 33
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobjgen/src/SOxIntfCBCode.m3,v $
 * $Date: 2001-12-03 17:23:37 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 13:15:54  wagner
 * Blair MacIntyre's sharedobjgen package
 *
 * Revision 1.4  1997/08/11 20:36:34  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

MODULE SOxIntfCBCode;

IMPORT SOxCoder, Atom, Formatter, SOxCodeUtils, SOxCodeFiles, Type,
       Wr, ImportList, CodeForType, AtomList;

REVEAL T = SOxCoder.T BRANDED OBJECT 
OVERRIDES
  InitImports := initImports;
  Import := import;
  Head := head;
  Decls := decls; 
  Main := noMain;
  Bottom := bottom;
END;

<* FATAL Wr.Failure*>

(* \subsection{Utility procedures and abreviations}
   All procedures that output code in this module use the "Formatter"
   module. The following abreviation is used by all procedures in order
   to get better readable program text:

   |    WITH Put    = Formatter.PutText,
   |         Nl     = Formatter.NewLine,
   |         Tab    = Formatter.Begin,
   |         EndTab = Formatter.End      DO

   Often used "Formatter"-procedure sequences are combined in the
   procdures "PutLine".
*)

PROCEDURE PutLine (fmtWr: Formatter.T; text: TEXT) =
  BEGIN
    Formatter.PutText(fmtWr, text);
    Formatter.NewLine(fmtWr, freshLine := FALSE);
  END PutLine;

VAR
  extraImports := ARRAY [1..1] OF Atom.T{Atom.FromText("SharedObj")};

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
    CodeForType.ImportCBLst(type, imports, methods, umethods);
  END import;

PROCEDURE head(self: T;
               wr: Formatter.T;
               filename: TEXT;  
               basename: TEXT; 
               imports: ImportList.T) =
  BEGIN
    WITH Nl     = Formatter.NewLine DO
      SOxCodeUtils.HeaderComment(wr, filename);
      self.fbasename := SOxCodeUtils.FileName(basename, SOxCodeFiles.T.CB_I3);

      PutLine(wr, "INTERFACE " & self.fbasename & ";\n");
      CodeForType.ProduceImports(wr, imports);

      Nl(wr, freshLine := FALSE); 
    END;
  END head;

PROCEDURE decls(<*UNUSED*>self: T;
                wr: Formatter.T;
                typeID: Type.Qid;  
                <*UNUSED*>stypeID: Type.Qid;  
                <*UNUSED*>implName: TEXT; 
                methods: ImportList.MethodList;
                umethods: AtomList.T) =
  VAR typTxt : TEXT;
      identfTxt : TEXT;
      meth: Atom.T;
  BEGIN
    WITH Put    = Formatter.PutText,
         Nl     = Formatter.NewLine,
         Tab    = Formatter.Begin,
         EndTab = Formatter.End     DO
      typTxt := CodeForType.QidToText(typeID);
      identfTxt := CodeForType.QidToIdentf(typeID);

      Tab(wr, 2);
      Put(wr, "TYPE"); 

      Nl(wr, freshLine := FALSE); 
      PutLine(wr, identfTxt & " <: Public" & identfTxt & ";");
      Tab(wr, 2);
      PutLine(wr, "Public" & identfTxt & " = SharedObj.Callback OBJECT");
      Tab(wr, 2);
      PutLine(wr, "METHODS ");

      Put(wr,"init (");
      Tab(wr, 0);
      Put(wr,"obj: " & typTxt & "): " & identfTxt & ";");
      EndTab(wr);
      Nl(wr, freshLine := FALSE);

      PutLine(wr,"cancel ();");

      Put(wr,"pre_anyChange (");
      Tab(wr, 0);
      Put(wr,"READONLY obj: " & typTxt & ");"); 
      EndTab(wr);
      Nl(wr, freshLine := FALSE);

      Put(wr,"post_anyChange (");
      Tab(wr, 0);
      Put(wr,"READONLY obj: " & typTxt & ");");  
      EndTab(wr);

      FOR i := 0 TO LAST(methods^) DO
        meth := methods[i].name;
        IF AtomList.Member(umethods, meth) THEN 
          Nl(wr, freshLine := FALSE);

          Put(wr, "pre_" & Atom.ToText(meth) & " (");
          Tab(wr, 0);
          Put(wr, "READONLY obj: " & typTxt);
          CodeForType.PrintSig(wr, methods[i].sig);
          Put(wr, "): BOOLEAN;");
          EndTab(wr);
          Nl(wr, freshLine := FALSE);

          Put(wr, "post_" & Atom.ToText(meth) & " (");
          Tab(wr, 0);
          Put(wr, "READONLY obj: " & typTxt);
          CodeForType.PrintSig(wr, methods[i].sig);
          Put(wr, "): BOOLEAN;");
          EndTab(wr);
        END;
      END;
      EndTab(wr);
      Nl(wr, freshLine := FALSE);      
      PutLine(wr, "END;");
      EndTab(wr);

      EndTab(wr);
      Nl(wr, freshLine := FALSE);      
    END
 END decls;
 
PROCEDURE noMain(<*UNUSED*> self: T;
                 <*UNUSED*> wr: Formatter.T;
                 <*UNUSED*> typeID: Type.Qid;  
                 <*UNUSED*> type    : Type.Object;  
                 <*UNUSED*> stypeID: Type.Qid;  
                 <*UNUSED*> implName: TEXT; 
                 <*UNUSED*> methods: ImportList.MethodList;
                 <*UNUSED*> umethods: AtomList.T) = 
BEGIN
END noMain;

PROCEDURE bottom(self: T;
                 wr: Formatter.T; 
                 <*UNUSED*>fname: TEXT) =
  BEGIN
    PutLine(wr, "END " & self.fbasename & ".");
  END bottom;


BEGIN   
END SOxIntfCBCode.

