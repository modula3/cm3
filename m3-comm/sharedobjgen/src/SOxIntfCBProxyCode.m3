(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Tobias Hoellerer (htobias)
 * Created On      : Fri Nov 10 17:37:04 EST 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Sep 25 09:48:26 1997
 * Update Count    : 35
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  1997/10/24 19:19:26  bm
 * New files.
 *
 * Revision 1.3  1997/08/11 20:36:36  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

MODULE SOxIntfCBProxyCode;

IMPORT SOxCoder, Atom, Formatter, SOxCodeUtils, SOxCodeFiles, Type,
       Wr, ImportList, CodeForType, AtomList;

REVEAL T = SOxCoder.T BRANDED OBJECT 
OVERRIDES
  InitImports := initImports;
  Import := import;
  Head := head;
  Decls := decls; 
  Main := main;
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
  extraImports := ARRAY [1..1] OF Atom.T{Atom.FromText("EmbProxiedObj")};

PROCEDURE initImports(<*UNUSED*>self: T;
                      basename: TEXT; 
                      imports: ImportList.T) =
  VAR imp := ARRAY [1..1] OF Atom.T{
    Atom.FromText(SOxCodeUtils.FileName(basename, SOxCodeFiles.T.CB_I3))};
  BEGIN
    CodeForType.AugmentImportList(imports, extraImports);
    CodeForType.AugmentImportList(imports, imp);
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
    SOxCodeUtils.HeaderComment(wr, filename);
    self.basename := basename;
    self.fbasename := SOxCodeUtils.FileName(basename, SOxCodeFiles.T.CBPRX_I3);

    PutLine(wr, "INTERFACE " & self.fbasename & ";\n");
    CodeForType.ProduceImports(wr, imports);
  END head;

PROCEDURE decls(self: T;
                wr: Formatter.T;
                typeID: Type.Qid;  
                <*UNUSED*>stypeID: Type.Qid;  
                <*UNUSED*>implName: TEXT; 
                <*UNUSED*>methods: ImportList.MethodList;
                <*UNUSED*>umethods: AtomList.T) = 
  VAR typTxt : TEXT;
      identfTxt : TEXT;
  BEGIN
    WITH Nl     = Formatter.NewLine,
         Tab    = Formatter.Begin,
         EndTab = Formatter.End     DO
      typTxt := CodeForType.QidToText(typeID);
      identfTxt := CodeForType.QidToIdentf(typeID);

      Nl(wr, freshLine := FALSE); 
      Tab(wr, 2);
      PutLine(wr, "VAR"); 
(*
      PutLine(wr, "MkProxy" & identfTxt & " : PROCEDURE(x: " &
        typTxt & ") := NIL;");  
*)
      PutLine(wr, "MkProxy" & identfTxt & "CB : PROCEDURE(x: " &
        SOxCodeUtils.FileName(self.basename, SOxCodeFiles.T.CB_I3) & "." &
        identfTxt & ") := NIL;");        

      EndTab(wr);
    END
 END decls;
 
PROCEDURE main(<*UNUSED*>self: T;
               wr: Formatter.T;
               typeID: Type.Qid;  
               <*UNUSED*>type: Type.Object;  
               <*UNUSED*>stypeID: Type.Qid;  
               <*UNUSED*>implName: TEXT; 
               methods: ImportList.MethodList;
               umethods: AtomList.T) =
  VAR typTxt : TEXT;
      identfTxt : TEXT;
      meth : Atom.T;
  BEGIN
    WITH Put    = Formatter.PutText,
         Nl     = Formatter.NewLine,
         Tab    = Formatter.Begin,
         EndTab = Formatter.End     DO
      typTxt := CodeForType.QidToText(typeID);
      identfTxt := CodeForType.QidToIdentf(typeID);

      Nl(wr, freshLine := FALSE); 
      Tab(wr, 2);
      PutLine(wr, "TYPE");
      Tab(wr, 2);
      PutLine(wr, "CBProxy" & identfTxt & 
        " = EmbProxiedObj.Proxy OBJECT METHODS");

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
    END;
  END main;

PROCEDURE bottom(self: T;
                 wr: Formatter.T; 
                 <*UNUSED*>fname: TEXT) =
  BEGIN
    WITH  Nl     = Formatter.NewLine DO
      Nl(wr, freshLine := FALSE);      
      PutLine(wr, "END " & self.fbasename & ".");
    END;
  END bottom;


BEGIN   
END SOxIntfCBProxyCode.
