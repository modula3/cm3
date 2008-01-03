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
 * Last Modified On: Sat Aug  9 13:54:12 1997
 * Update Count    : 11
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobjgen/src/SOxModuleProxyCode.m3,v $
 * $Date: 2001-12-03 17:23:37 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 13:15:54  wagner
 * Blair MacIntyre's sharedobjgen package
 *
 * Revision 1.3  1997/08/11 20:36:39  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

MODULE SOxModuleProxyCode;

IMPORT SOxCodeUtils, SOxCoder, Formatter, ImportList, SOxCodeGenError,
       Type, SOxCodeFiles, Wr, CodeForType, Atom, AtomList; 

REVEAL T = SOxCoder.T BRANDED OBJECT 
OVERRIDES
  Head := head;
  Decls := decls;
  Main := main;
  Bottom := bottom;
END;

<* FATAL Wr.Failure*>

PROCEDURE PutLine (fmtWr: Formatter.T; text: TEXT) =
  BEGIN
    Formatter.PutText(fmtWr, text);
    Formatter.NewLine(fmtWr, freshLine := FALSE);
  END PutLine;


PROCEDURE head(self: T;
               wr: Formatter.T; 
               fname: TEXT; 
               basename: TEXT; 
               <*UNUSED*>imports: ImportList.T) =
  BEGIN
    WITH Put    = Formatter.PutText,
         Nl     = Formatter.NewLine DO
      SOxCodeUtils.HeaderComment(wr, fname);
      self.basename := basename;
      self.fbasename := SOxCodeUtils.FileName(basename, SOxCodeFiles.T.PRX_M3);
      PutLine(wr, "MODULE " & self.fbasename  & ";");
      Nl(wr, freshLine := FALSE);
      Put(wr, "IMPORT " & basename & ";");
      Nl(wr, freshLine := FALSE);
      Nl(wr, freshLine := FALSE); 
      PutLine(wr, "REVEAL"); 
    END;
  END head;

PROCEDURE decls(<*UNUSED*>self: T;
                wr: Formatter.T; 
                typeID: Type.Qid;  
                <*UNUSED*>implName: TEXT; 
                methods: ImportList.MethodList;
                umethods: AtomList.T) =
  BEGIN
  VAR typTxt : TEXT;
      identfTxt : TEXT;
      meth: Atom.T;
  BEGIN
    WITH Put    = Formatter.PutText,
         Nl     = Formatter.NewLine,
         Tab    = Formatter.Begin,
         Grp    = Formatter.Group,
         EndTab = Formatter.End     DO
      typTxt := CodeForType.QidToText(typeID);
      identfTxt := CodeForType.QidToIdentf(typeID);
      Tab(wr, 1);
      Nl(wr, freshLine := FALSE);
      Tab(wr, 1); 
      PutLine(wr, identfTxt & " = Public_" & identfTxt & " BRANDED " 
      & "Brand_" & identfTxt & " OBJECT;");
      PutLine(wr,"item: " & typTxt & ";");
      EndTab(wr);
      Tab(wr, 1); 
      PutLine(wr,"OVERRIDES");

      PutLine(wr,"init := Init_" & identfTxt & ";");
      FOR i := 0 TO LAST(methods^) DO
        meth := methods[i].name;
        IF NOT AtomList.Member(umethods, meth) THEN 
          Nl(wr, freshLine := FALSE);
          Grp(wr);
          Put(wr, Atom.ToText(meth) & " := My_" 
          & Atom.ToText(meth) & "_" & identfTxt & ";");  
          EndTab(wr);
        END;
      END;
      EndTab(wr);
      Nl(wr, freshLine := FALSE);      
      PutLine(wr, "END;");
      EndTab(wr);
      Nl(wr, freshLine := FALSE);      
    END;
  END;
END decls;


PROCEDURE main(<*UNUSED*>self: T;
               wr: Formatter.T; 
               typeID: Type.Qid;  
               type: Type.Object;  
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
      Nl(wr, freshLine := FALSE);

      Tab(wr,1);
      PutLine(wr, "PROCEDURE Init_" & identfTxt & "( self: " & identfTxt 
      & "; " & "t: " & typTxt & ") :" & identfTxt & " ="); 
      Tab(wr,1);
      PutLine(wr, "BEGIN");
      PutLine(wr, "self.item := t;"); 
      EndTab(wr);
      PutLine(wr, "RETURN self;"); 
      EndTab(wr);
      PutLine(wr, "END Init_" &  identfTxt & ";");

      FOR i := 0 TO LAST(methods^) DO
        meth := methods[i].name;
        IF NOT AtomList.Member(umethods, meth) THEN 
          Nl(wr, freshLine := FALSE);
          Tab(wr,1);
          Put(wr, "PROCEDURE My_" & Atom.ToText(meth) & "_" &
            identfTxt & "( self: " & identfTxt & "; ");
          CodeForType.PrintSig(wr, methods[i].sig);
          IF methods[i].sig.result # NIL THEN
            PutLine(wr, "): " & typTxt & " =");
          ELSE
            PutLine(wr, ") =");
          END;
          Tab(wr,1);
          PutLine(wr, "BEGIN");
          EndTab(wr);
          PutLine(wr, "RETURN self.item;"); 
          EndTab(wr);
          PutLine(wr, "END My_" & Atom.ToText(methods[i].name) & "_" &
            identfTxt & ";");

        END;
      END;
    END;
  END main;

PROCEDURE bottom(self: T;
                 wr: Formatter.T; 
                 <*UNUSED*>fname: TEXT) =
  BEGIN
    WITH  Nl     = Formatter.NewLine DO
      Nl(wr, freshLine := FALSE);
      PutLine(wr, "BEGIN");
      PutLine(wr, "END " & self.fbasename & ".");
    END;
  END bottom;
  

BEGIN
END SOxModuleProxyCode.
