(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Feb  1 09:44:24 PST 1995 by kalsow *)
(*      modified on Sat Sep 10 14:24:12 PDT 1994 by bharat *)
(*      modified on Wed Nov 17 16:09:19 PST 1993 by mhb    *)
<* PRAGMA LL *>

MODULE NodeVBT EXPORTS NodeVBT;

IMPORT ASCII, Attributes, Axis, Dialog, DialogMenu, Fmt,
       FormsVBT, GenerateObliq, Lex, ListVBT,
       Point, Pts, RW, Rect, Rd, Rsrc, Split, Stdio, Text, TextRd, Thread,
       TSplit, VBT, Wr, ZHandleVBT, ZSplit;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented, InvalidObjectName,
         Rd.Failure, Split.NotAChild, Thread.Alerted *>


REVEAL
  T = Public BRANDED "VO-NodeVBT" OBJECT
      OVERRIDES
        loadAttributes             := LoadAttributes;
        checkAttributes            := CheckAttributes;
        applyAttributes            := ApplyAttributes;
        computeSX                  := ComputeSX;
        generateObjectDefs         := GenerateObjectDefs;
        generateCallbacks          := GenerateCallbacks;
        generateAttachments        := GenerateAttachments;
        generateInitializationCode := GenerateInitializationCode;
        SXTemplate                 := SX;
        save                       := Save;
        load                       := Load;
        initObliqAttrs       := InitObliqAttrs;
      END;

  Widget = PublicWidget BRANDED "VO-Widget" OBJECT END;

  SplitNode =
    PublicSplit BRANDED "VO-SplitNode" OBJECT
      Texture := "blank.pbm";
    OVERRIDES
      loadAttributes := SplitLoadAttributes;
      (* checkAttributes := SplitCheckAttributes;*)
      applyAttributes            := SplitApplyAttributes;
      computeSX                  := SplitComputeSX;
      generateObjectDefs         := SplitGenerateObjectDefs;
      generateCallbacks          := SplitGenerateCallbacks;
      generateAttachments        := SplitGenerateAttachments;
      generateInitializationCode := SplitGenerateInitializationCode;
      save                       := SplitSave;
      load                       := SplitLoad;
      initObliqAttrs       := SplitObAttrs;
    END;

  FormNode =
    PublicForm BRANDED "VO-PublicForm" OBJECT

      Tag: BOOLEAN;
      (* used to eliminate nodes in the subtree rooted at a given node from
         the list of possible parent-nodes *)

      TitleBgColor: TEXT     := "VerySlightlyBluishGrey85";
      TitleFgColor: TEXT     := "Black";
      TitleString : TEXT     := "Title";
      StretchX    : CARDINAL := 1000;
      StretchY    : CARDINAL := 1000;
      ShrinkX     : CARDINAL := 0;
      ShrinkY     : CARDINAL := 0;

    OVERRIDES
      getDomain                  := GetDomain;
      loadAttributes             := FormLoadAttributes;
      applyAttributes            := FormApplyAttributes;
      computeSX                  := FormComputeSX;
      generateObjectDefs         := FormGenerateObjectDefs;
      generateCallbacks          := FormGenerateCallbacks;
      generateAttachments        := FormGenerateAttachments;
      generateInitializationCode := FormGenerateInitializationCode;

      save := FormSave;
      load := FormLoad;
      initObliqAttrs       := FormObAttrs;
    END;

  FrameNode = PublicFrame BRANDED "VO-FrameNode" OBJECT END;

(*****************************************************************************)
(* Variables for object management *)
TYPE
  ObjList = ARRAY [0 .. 100] OF T;
  ObjClass = RECORD
               name      : TEXT;  (* name of the class *)
               instances          := 0; (* number of instances *)
               count              := 0;
               SXTemplate: TEXT;

               instanceList: ObjList;

               (*-- the following dont need to be saved *)
               createProc                     : Proc;
               last                           : CARDINAL  := 0;
               minParentWidth, minParentHeight: INTEGER;
               attrsheetName: TEXT;  (* this is generally name & "att" but
                                        in some cases multiple widgets may
                                        share the same attr sheet - e.g
                                        hscroll/vscroll share the attr
                                        sheet scrolleratt *)
          
             END;

  
(* This is the createproc used by the Manager to create new object
   instances *)
(* They are registered by widgets using the register method *)


VAR
  ObjectClasses: REF ARRAY [0 .. 100] OF ObjClass;
  ClassCounter : CARDINAL                         := 0;
  (* number of classes installed *)
  inited        : BOOLEAN   := FALSE;

  infoList :  ARRAY [0..100] OF REF InfoDefn;
  createdInfoList : BOOLEAN := FALSE; 
  infoCtr := 0;

PROCEDURE SX (nv: T): TEXT =
  BEGIN
    RETURN ObjectClasses[nv.classIndex].SXTemplate;
  END SX;

PROCEDURE LoadAttributes (nv: T; as: FormsVBT.T) =
  BEGIN
    (* load generic attributes *)
    (* Name *)
    FormsVBT.PutText(as, "name", nv.name, FALSE);
    (* Type *)
    FormsVBT.PutText(as, "type", GetNodeTypeName(nv), FALSE);
    (* FgColor *)
    FormsVBT.PutText(as, "fgctypein", nv.FgColor, FALSE);
    (* BgColor *)
    FormsVBT.PutText(as, "bgctypein", nv.BgColor, FALSE);
    (* Rim size *)
    FormsVBT.PutInteger(as, "rimsize", nv.Rim);
    (* Border size *)
    FormsVBT.PutInteger(as, "bordersize", nv.Border);
    (* Font *)
    FormsVBT.PutText(as, "fonttypein", nv.Font, FALSE);
    (* Embellishment *)
    FormsVBT.PutChoice(as, "Embellishment", nv.Embellishment);
    (* Reactivity *)
    FormsVBT.PutChoice(as, "InitialState", nv.InitialState);
    (* Reshape *)
    FormsVBT.PutChoice(as, "Reshape", nv.ResizeModel);
    (* Foreground / Background *)
    IF nv.Foreground THEN
      FormsVBT.PutChoice(as, "exechow", "Foreground")
    ELSE
      FormsVBT.PutChoice(as, "exechow", "Background")
    END;
    IF nv.Local THEN
      FormsVBT.PutChoice(as, "execwhere", "Local")
    ELSE
      FormsVBT.PutChoice(as, "execwhere", "Remote");
      FormsVBT.MakeActive(as, "remFilter");
    END;
    
    FormsVBT.PutText(as, "Location", nv.Location, FALSE);
    FormsVBT.PutText(as, "CallbackEditor", nv.Code, FALSE);
    Attributes.currentNode := nv;
  END LoadAttributes;

PROCEDURE IntAttr(name:TEXT; arg : INTEGER): TEXT =
  BEGIN
    RETURN  "\ttemp." & name & " := " & Fmt.Int(arg) & ";\n";
  END IntAttr;

PROCEDURE TextAttr(name:TEXT; arg :TEXT) : TEXT =
  BEGIN
    RETURN  "\ttemp." & name & " := \"" &  GenerateObliq.SlashQuotes(
                                               GenerateObliq.SlashSlashes(arg)) & "\";\n";
  END TextAttr;

PROCEDURE BoolAttr(name:TEXT; arg :BOOLEAN) : TEXT =
  BEGIN
    IF arg THEN
      RETURN  "\ttemp." & name & " := true;\n";
    ELSE
      RETURN  "\ttemp." & name & " := false;\n";
    END; 
  END BoolAttr;



PROCEDURE InitObliqAttrs(v:T) : TEXT =
  VAR ret := "temp.SELF := meth(s) VOInstance end;\n";
  BEGIN
    IF (v.parent # NIL) THEN
      ret := ret &  "\ttemp.parent := meth(s) VOInstance."
      & v.parent.name & "  end;\n";
    END;

     ret := ret & IntAttr("x", v.x) &
                IntAttr("y", v.y) & 
                IntAttr("width", v.width) & 
                IntAttr("height", v.height); 

     ret := ret & TextAttr("BgColor", v.BgColor) &
                TextAttr("FgColor", v.FgColor) &
                TextAttr("Font", v.Font) &
                IntAttr("Rim", v.Rim) &
                IntAttr("Border", v.Border) &
                TextAttr("Embellishment", v.Embellishment) &
                TextAttr("InitialState", v.InitialState) &
                TextAttr("ResizeModel", v.ResizeModel);
     RETURN ret;
    
   END InitObliqAttrs; 

PROCEDURE CheckAttributes (v: T; as: FormsVBT.T; VAR error: TEXT):
  BOOLEAN =
  BEGIN
    (* check the validity of generic attributes *)
    WITH nom   = FormsVBT.GetText(as, "name"),
         other = GetNodeNamed(nom)             DO (* search all lists *)
      IF other # NIL AND other # v THEN
        error := "There is already an object called " & nom;
        RETURN FALSE;
      END
    END;
    RETURN TRUE;
  END CheckAttributes;


PROCEDURE ComputeSX (v: T; Final: BOOLEAN := FALSE): TEXT =
  VAR start, found: INTEGER;
  BEGIN
    (* this is called last after all overrides have finished with the
       s-expression*)
    v.DialogSX := FindAndReplace(v.DialogSX, "XSpan", Fmt.Int(v.width));
    v.DialogSX := FindAndReplace(v.DialogSX, "YSpan", Fmt.Int(v.height));
    v.DialogSX := FindAndReplace(v.DialogSX, "RimPen", Fmt.Int(v.Rim));
    v.DialogSX :=
      FindAndReplace(v.DialogSX, "BorderPen", Fmt.Int(v.Border));
    v.DialogSX := FindAndReplace(v.DialogSX, "BgColor", v.BgColor);
    v.DialogSX := FindAndReplace(v.DialogSX, "FgColor", v.FgColor);
    v.DialogSX := FindAndReplace(v.DialogSX, "Font", v.Font);
  
    IF Final THEN
      v.DialogSX :=
        FindAndReplace(v.DialogSX, "FilterState", v.InitialState)
    ELSE
      v.DialogSX := FindAndReplace(v.DialogSX, "FilterState", "Active")
    END;

    IF Text.Equal(v.Embellishment, "None") THEN
      v.DialogSX :=
        FindAndReplace(v.DialogSX, "FrameStyle", "(ShadowSize 0)");
      (* Remember to set it to 1.5 in the next level if a shadow is needed
         within *)
    ELSE
      v.DialogSX :=
        FindAndReplace(v.DialogSX, "FrameStyle", v.Embellishment);
    END;
    

    (* now replace all %@ occurences with %v.name *)
    start := 0;
    found := Text.FindChar(v.DialogSX, '%', start);
    WHILE found # -1 AND found + 2 < Text.Length(v.DialogSX) DO
      IF Text.GetChar(v.DialogSX, found + 1) = '@' THEN
        v.DialogSX := Text.Sub(v.DialogSX, 0, found + 1) & v.name
                        & Text.Sub(v.DialogSX, found + 2)
      END;
      start := found + 2;
      found := Text.FindChar(v.DialogSX, '%', start);
    END;
    (* print("After ComputeSX :\n" & v.DialogSX & "\n"); *)
    TRY
      FOR i := 1 TO Dialog.screens DO
        WITH fv = Dialog.screen[i] DO
          FormsVBT.PutText(fv, "sxview", v.DialogSX, FALSE)
        END
      END
    EXCEPT
    ELSE
    END;
    RETURN v.DialogSX;
  END ComputeSX;

PROCEDURE ApplyAttributes (v: T; as: FormsVBT.T) =
  BEGIN
    (* Apply generic attributes - this is overriden in all cases.  All
       widgets call this before applying their own attributes Split nodes
       do not call this method - forms and frames call the split node
       method *)
    v.name := FormsVBT.GetText(as, "name");
    v.FgColor := FormsVBT.GetText(as, "fgctypein");
    v.BgColor := FormsVBT.GetText(as, "bgctypein");
    v.Rim := FormsVBT.GetInteger(as, "rimsize");
    v.Border := FormsVBT.GetInteger(as, "bordersize");
    v.Font := FormsVBT.GetText(as, "fonttypein");
    v.Embellishment := FormsVBT.GetChoice(as, "Embellishment");
    v.ResizeModel := FormsVBT.GetChoice(as, "Reshape");
    v.InitialState := FormsVBT.GetChoice(as, "InitialState");
    v.Foreground :=
      Text.Equal(FormsVBT.GetChoice(as, "exechow"), "Foreground");
    v.Local := Text.Equal(FormsVBT.GetChoice(as, "execwhere"), "Local");
    v.Location := FormsVBT.GetText(as, "Location");
    v.Code := FormsVBT.GetText(as, "CallbackEditor");

    (* Inheritance *)
    IF Text.Equal(v.FgColor, "Inherit") THEN
      IF ISTYPE(v, FormNode) THEN (* get global value *)
        v.FgColor := defaultFgColor;
      ELSE
        v.FgColor := v.parent.FgColor;
      END;
      FormsVBT.PutText(as, "fgctypein", v.FgColor, FALSE);
    END;
    IF Text.Equal(v.BgColor, "Inherit") THEN
      IF ISTYPE(v, FormNode) THEN (* get global value *)
        v.BgColor := defaultBgColor;
      ELSE
        v.BgColor := v.parent.BgColor;
      END;
      FormsVBT.PutText(as, "bgctypein", v.BgColor, FALSE);
    END;
    IF Text.Equal(v.Font, "Inherit") THEN
      IF ISTYPE(v, FormNode) THEN (* get global value *)
        v.Font := defaultFont;
      ELSE
        v.Font := v.parent.Font;
      END;
      FormsVBT.PutText(as, "fonttypein", v.Font, FALSE);
    END;

    ComputeDimensions(v);

  END ApplyAttributes;


PROCEDURE GenerateObjectDefs (nv: T): TEXT =
  VAR code := nv.name & " => ( let temp = LOCAL.";
  BEGIN
   code := code & GetNodeTypeName(nv) & "New(\""
             & nv.name & "\");\n";
   code := code & "\ttemp.form := meth(s) SELF.FORM end;\n";
   code := code & nv.initObliqAttrs();

   code := code & "temp),\n";

    RETURN code;
  END GenerateObjectDefs;

PROCEDURE GenerateCallbacks (nv: T): TEXT =
  VAR ncb: TEXT;
  BEGIN
    IF AllWhitespace(nv.Code) THEN RETURN ""; END;

    ncb :=
      FindAndReplace(GenerateObliq.callbackTemplate, "objname", nv.name);
    IF nv.Foreground THEN
      ncb := FindAndReplace(ncb, "bgHeader", "");
      ncb := FindAndReplace(ncb, "bgFooter", "")
    ELSE
      ncb := FindAndReplace(ncb, "bgHeader", "thread_fork(proc()\n");
      ncb := FindAndReplace(ncb, "bgFooter", "\nend, 10000)\n")
    END;

    IF nv.Local THEN
      ncb := FindAndReplace(ncb, "remoteHeader", "");
      ncb := FindAndReplace(ncb, "remoteFooter", "")
    ELSE
      ncb :=
        FindAndReplace(
          ncb, "remoteHeader",
          "let VODest = \n(*----------------------------------------*)\n"
            & nv.Location & ";\n"
            & "(*----------------------------------------*)\n"
            & "VODest.VOCompute( proc(REMOTE) \n");
      ncb := FindAndReplace(ncb, "remoteFooter", "\n ok; \n end )\n");
    END;

    RETURN FindAndReplace(ncb, "usercode",
                          "(* Callback for " & nv.name & "*)\n" & nv.Code)
             & "\n";
  END GenerateCallbacks;

PROCEDURE GenerateAttachments (nv: T): TEXT =
  BEGIN
    (* this is not a split node *)
    IF AllWhitespace(nv.Code) THEN RETURN ""; END;
    RETURN "form_attach(SELF.FORM,  SELF." & nv.name & ".name, SELF." & nv.name
             & "Proc);\n";
  END GenerateAttachments;

PROCEDURE GenerateInitializationCode (nv: T): TEXT =
  BEGIN
    RETURN ("(* Initialization Code " & nv.name & " *)\n");
  END GenerateInitializationCode;

PROCEDURE ComputeDimensions (nv: T) =
  VAR pardomain: Rect.T;
  BEGIN

    WITH v      = nv.getchild(),
         dom    = VBT.Domain(v),
         nw     = Rect.NorthWest(dom),
         width  = Rect.HorSize(dom),
         height = Rect.VerSize(dom)    DO
      (* convert from Pixels to Points *)

      nv.width := ROUND(FLOAT(width) / Pts.ToPixels(v, 1.0, Axis.T.Hor));
      nv.height := ROUND(FLOAT(height) / Pts.ToPixels(v, 1.0, Axis.T.Ver));

      IF NOT ISTYPE(nv, FormNode) THEN
        pardomain := VBT.Domain(nv.parent)
      ELSE
        WITH fn     = NARROW(nv, FormNode),
             dialog = Dialog.screen[fn.Screen],
             zsplit = FormsVBT.GetVBT(dialog, "topZSplit") DO
          pardomain := VBT.Domain(zsplit)
        END
      END;
      WITH parnw = Rect.NorthWest(pardomain) DO
        nv.x :=
          ROUND(FLOAT(nw.h - parnw.h) / Pts.ToPixels(v, 1.0, Axis.T.Hor));
        nv.y :=
          ROUND(FLOAT(nw.v - parnw.v) / Pts.ToPixels(v, 1.0, Axis.T.Ver));
      END


      (*
        nv.x := ROUND(FLOAT(nw.h) / Pts.ToPixels(v, 1.0, Axis.T.Hor));
        nv.y := ROUND(FLOAT(nw.v) / Pts.ToPixels(v, 1.0, Axis.T.Ver));
      *)
    END;
    print("ComputeDimensions of  " & nv.name & " ::=  " & Fmt.Int(nv.x)
            & "," & Fmt.Int(nv.y) & ":" & Fmt.Int(nv.width) & ","
            & Fmt.Int(nv.height) & "\n");
  END ComputeDimensions;

PROCEDURE LRProc (<* UNUSED *> cl  : FormsVBT.Closure;
                               afv : FormsVBT.T;
                               name: TEXT;
                  <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    IF Text.Equal(name, "Local") THEN
      FormsVBT.MakeDormant(afv, "remFilter")
    ELSE
      FormsVBT.MakeActive(afv, "remFilter")
    END
  END LRProc;

(**********************Split  Attribute Management *******************)
PROCEDURE SplitLoadAttributes (nv: SplitNode; as: FormsVBT.T) =
  BEGIN
    (* load texture attributes *)
    (* Name *)
    IF ISTYPE(nv, FormNode) THEN
      FormsVBT.PutText(as, "formtexture", nv.Texture, FALSE);
    ELSE
      FormsVBT.PutText(as, "frametexture", nv.Texture, FALSE);
    END;
    LoadAttributes(nv, as);      (* common attributes *)
  END SplitLoadAttributes;


(*
PROCEDURE SplitCheckAttributes (    nv   : SplitNode;
                                    as   : FormsVBT.T;
                                    VAR error: TEXT        ): BOOLEAN =
  VAR texture: TEXT;
  BEGIN
    (* check the texture filename attributes *)
    IF ISTYPE(nv, FormNode) THEN
      texture := FormsVBT.GetText(as, "formtexture")
    ELSE
      texture := FormsVBT.GetText(as, "frametexture")
    END;
    IF Text.Equal(texture, "Blank") THEN texture := "blank.pbm" END;
    TRY
      cachedRawImage := GetRawImage(texture)
    EXCEPT
      Error (msg) => error := "Error in Texture." & msg; RETURN FALSE
    ELSE
      error := "Invalid Texture Specification";
      RETURN FALSE
    END;
    RETURN CheckAttributes(nv, as, error);
  END SplitCheckAttributes;
*)
PROCEDURE SplitApplyAttributes (nv: SplitNode; as: FormsVBT.T) =
  BEGIN
    ApplyAttributes(nv, as);
    IF ISTYPE(nv, FormNode) THEN
      nv.Texture := FormsVBT.GetText(as, "formtexture")
    ELSE
      nv.Texture := FormsVBT.GetText(as, "frametexture")
    END
  END SplitApplyAttributes;


PROCEDURE SplitObAttrs(nv: SplitNode) : TEXT =
  VAR ret :=  TextAttr("Texture", nv.Texture);
  BEGIN
    (* do all the kids of the split *)
    ret := ret & "\ttemp.children := meth(s)  [";
    (* iterate over children and put in a VOInstance.<child.name> for each child *)
    FOR i := 1 TO nv.nc DO
      WITH child = nv.children[i] DO
        ret := ret & "VOInstance." & child.name & ", ";
      END (* WITH *)
    END;
    ret := ret & "]; end;\n";
   RETURN InitObliqAttrs(nv) & ret;
  END SplitObAttrs; 


PROCEDURE ResizeString(ch : T) : TEXT =
VAR
  model := ch.ResizeModel;
  retval := " Scaled"; (* default *)
  BEGIN
    IF Text.Equal(model, "CenterPin") THEN
      retval := " FixedHV "
    ELSIF Text.Equal(model, "HScaled") THEN
      retval := " FixedV "
    ELSIF Text.Equal(model, "VScaled") THEN
      retval := " FixedH "
    END;
    RETURN retval;
  END ResizeString;

PROCEDURE SplitComputeSX (nv: SplitNode; Final: BOOLEAN := FALSE)
  : TEXT =
  VAR footer := ")\n";
      child_offset_redn := 0;
      include_title_bar := TRUE;
  BEGIN
    IF NOT Final THEN
      nv.DialogSX := FindAndReplace(nv.DialogSX, "ZSplitHeader", "");
      nv.DialogSX := FindAndReplace(nv.DialogSX, "ZSplitFooter", "");
    ELSE
      nv.DialogSX :=
        FindAndReplace(nv.DialogSX, "ZSplitHeader",
                       "(ZSplit %@zsplit\n (ZBackground %@zback\n  ");

      IF ISTYPE(nv, FormNode)  THEN
            WITH  fn =  NARROW(nv, FormNode) DO
              IF fn.ParentForm = NIL THEN (* its a top level form .. *)
                child_offset_redn := GetTitleHt(fn);
                nv.DialogSX := FindAndReplace(nv.DialogSX, "YSpan", Fmt.Int(nv.height- child_offset_redn) );
                include_title_bar := FALSE;
              END (* IF *)
            END (* WITH *)
          END;

      FOR i := 1 TO nv.nc DO
        WITH child = nv.children[i] DO
          ComputeDimensions(child);
          child.DialogSX := child.SXTemplate();
          EVAL child.computeSX(TRUE);

          (* Attach child to the split .
             Top-level forms are a special case because the title bar has to be removed
             which means reducing the vert offset of all children and reducing the height
             of the form by the title-ht*)

      
          footer := footer & "(ZChild (At " & Fmt.Int(child.x) & "  "
                      & Fmt.Int(child.y - child_offset_redn) & "  NW Absolute) Open\n  " &
                      ResizeString(child) &
                      child.DialogSX & "\n    )\n";

        
        END
      END;

      IF ISTYPE(nv, FormNode) THEN 

        WITH fn = NARROW(nv, FormNode) DO (* do all the anchored children *)
          FOR j := 0 TO fn.NoOfChildren - 1 DO
            fn.ChildForms[j].DialogSX := fn.ChildForms[j].SXTemplate();
            ComputeDimensions(fn.ChildForms[j]);
            EVAL fn.ChildForms[j].computeSX(TRUE);
            footer := footer & fn.ChildForms[j].DialogSX;
          END;
        END (* WITH *);
       

      END (* IF *); 

      footer := footer & ")\n";
      nv.DialogSX := FindAndReplace(nv.DialogSX, "ZSplitFooter", footer);
    END (* IF *);

    
    nv.DialogSX := FindAndReplace(nv.DialogSX, "TextureFile", nv.Texture);
    (* print("After SplitComputeSX :\n" & nv.DialogSX & "\n"); *)

    IF ISTYPE(nv, FormNode) THEN
      IF include_title_bar THEN (* do title bar *)
        nv.DialogSX := FindAndReplace(nv.DialogSX, "IncludeTitleBar", "($TitleBar$)");
      ELSE
        nv.DialogSX := FindAndReplace(nv.DialogSX, "IncludeTitleBar", "");
      END;
      (* either way replace all occurences of $TitleBar$ with name & TitleBar *)
      nv.DialogSX := FAndRAll(nv.DialogSX, "TitleBar", nv.name & "TitleBar");
    END (* IF *);


    RETURN ComputeSX(nv, Final);
  END SplitComputeSX;

PROCEDURE SplitGenerateObjectDefs (nv: SplitNode): TEXT =
  VAR objdefs := "";
  BEGIN
    (* compute obj defs for all children *)
    FOR i := 1 TO nv.nc DO
      objdefs := objdefs & nv.children[i].generateObjectDefs();
    END;

    (* append obj def for this node *)
    RETURN objdefs & GenerateObjectDefs(nv);

  END SplitGenerateObjectDefs;

PROCEDURE SplitGenerateCallbacks (nv: SplitNode): TEXT =
  VAR cbdefs := "";
  BEGIN
    (* compute callbacks for all children *)
    FOR i := 1 TO nv.nc DO
      cbdefs := cbdefs & nv.children[i].generateCallbacks();
    END;

    (* Split Nodes have no callbacks *)
    RETURN cbdefs;
  END SplitGenerateCallbacks;

PROCEDURE SplitGenerateAttachments (nv: SplitNode): TEXT =
  VAR adefs := "";
  BEGIN
    (* compute attachments for all children *)
    FOR i := 1 TO nv.nc DO
      adefs := adefs & nv.children[i].generateAttachments();
    END;

    (* Split Nodes have no attachments *)
    RETURN adefs;
  END SplitGenerateAttachments;

PROCEDURE SplitGenerateInitializationCode (nv: SplitNode): TEXT =
  VAR prefix := "";
  BEGIN
    (* compute init code for all children *)
    FOR i := 1 TO nv.nc DO
      IF ISTYPE(nv.children[i], SplitNode) THEN
        prefix := prefix & nv.children[i].generateInitializationCode();
      END
    END;
    (* SplitNodes have no callbacks - the Code field has init code *)
    RETURN prefix & nv.Code;
  END SplitGenerateInitializationCode;

PROCEDURE print (c: TEXT) =
  BEGIN
    TRY
      Wr.PutText(Stdio.stdout, c);
      Wr.Flush(Stdio.stdout);
    EXCEPT
    ELSE
    END
  END print;

(************* Form Attribute Management ********************)

PROCEDURE ComputeAnchoredFormTree () =
  (* This computes the tree of anchored forms from the parent field of all
     the active FormNodes *)
  VAR
    formclass    := NameToIndex("form");
    n            := NoOfObjects(formclass);
    current  : T;
  BEGIN
   
    IF n > 0 THEN
      FOR i := 0 TO NoOfObjects(formclass) - 1 DO
        (* pass 1 - set counters to 0 *)
        IF i = 0 THEN
          current := GetFirst(formclass);
        
        ELSE
          current := GetNext(formclass);
         

        END;
        WITH z = NARROW(current, FormNode) DO
         
          z.NoOfChildren := 0;
          z.Tag := TRUE;         (* side effect *)
        END
      END;                       (* end of pass 1 *)
  
      FOR i := 0 TO NoOfObjects(formclass) - 1 DO
        IF i = 0 THEN
          current := GetFirst(formclass)
        ELSE
          current := GetNext(formclass)
        END;
       
        WITH z = NARROW(current, FormNode),
             p = z.ParentForm               DO
          IF p # NIL THEN
            
            p.ChildForms[p.NoOfChildren] := z;
            INC(p.NoOfChildren)
          END
        END
      END
    END                          (* end of pass 2 *)
  END ComputeAnchoredFormTree;

PROCEDURE ComputeViableParentsFor (nv: FormNode) =
  (* this calls ComputeAnchoredFormTree and tags nodes in the tree rooted
     at nv with Tag = FALSE *)

  PROCEDURE SetTagsOnTree (v: FormNode) =
    BEGIN
      v.Tag := FALSE;
      FOR i := 0 TO v.NoOfChildren - 1 DO
        SetTagsOnTree(v.ChildForms[i])
      END
    END SetTagsOnTree;

  BEGIN
    ComputeAnchoredFormTree();
    SetTagsOnTree(nv);
  END ComputeViableParentsFor;

PROCEDURE LoadParentNames (nv: FormNode; as: FormsVBT.T) =
  VAR
    formclass    := NameToIndex("form");
    n            := NoOfObjects(formclass);
    current  : T;
    list         := "";
  BEGIN
   
    ComputeViableParentsFor(nv);
    FOR i := 0 TO n - 1 DO
      IF i = 0 THEN
        current := GetFirst(formclass)
      ELSE
        current := GetNext(formclass)
      END;
      WITH z = NARROW(current, FormNode) DO
       
        IF z.Tag THEN list := list & z.name & "\n" END
      END
    END;
    WITH x       = FormsVBT.GetVBT(as, "parentlist"),
         parlist = NARROW(x, ListVBT.T)               DO
      Attributes.LVFlush(parlist);
      Attributes.LVAppendText(parlist, list);
    END;
  END LoadParentNames;

PROCEDURE AnchorProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                   afv : FormsVBT.T;
                      <* UNUSED *> name: TEXT;
                      <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    IF FormsVBT.GetBoolean(afv, "anchored") THEN
      LoadParentNames(NARROW(Attributes.currentNode, FormNode), afv);
      FormsVBT.PutText(afv, "parent", "", FALSE);
      FormsVBT.MakeActive(afv, "anchorfilter");
    ELSE
      FormsVBT.PutText(afv, "parent", "", FALSE);
      FormsVBT.MakeDormant(afv, "anchorfilter");
    END
  END AnchorProc;

PROCEDURE ParentProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                   afv : FormsVBT.T;
                      <* UNUSED *> name: TEXT;
                      <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    WITH nom = FormsVBT.GetTextProperty(afv, "parentlist", "Select") DO
      FormsVBT.PutText(afv, "parent", nom, FALSE);
    END;
    ZSplit.Unmap(FormsVBT.GetVBT(afv, "combo"));
    TSplit.SetCurrent(NARROW(FormsVBT.GetVBT(afv, "com"), TSplit.T),
                      FormsVBT.GetVBT(afv, "opencombo"));
  END ParentProc;

PROCEDURE MenuProc (<* UNUSED *> cl  : FormsVBT.Closure;
                                 afv : FormsVBT.T;
                                 name: TEXT;
                    <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    IF Text.Equal(name, "menuBoolean") THEN
      IF FormsVBT.GetBoolean(afv, "menuBoolean") THEN
        FormsVBT.MakeActive(afv, "tomenuFilter")
      ELSE
        FormsVBT.MakeDormant(afv, "tomenuFilter")
      END
    END
  END MenuProc;

PROCEDURE FormLoadAttributes (nv: FormNode; as: FormsVBT.T) =
  BEGIN

    (* Anchored *)
    (* If this is an anchored form set Anchored to true and enable the
       parent menu.  load the parent menu with possible parents i.e.  All
       Forms - Forms in the Anchor-Tree rooted at this node *)
    IF (nv.ParentForm # NIL) THEN
      FormsVBT.MakeActive(as, "anchorfilter");
      FormsVBT.PutText(as, "parent", nv.ParentForm.name, FALSE);
      FormsVBT.PutBoolean(as, "anchored", TRUE);
      LoadParentNames(nv, as);
    ELSE
      FormsVBT.PutBoolean(as, "anchored", FALSE);
      FormsVBT.PutText(as, "parent", "", FALSE);
      FormsVBT.MakeDormant(as, "anchorfilter");
    END;
    FormsVBT.PutBoolean(as, "menuBoolean", nv.HasMenu);
    IF nv.HasMenu THEN
      FormsVBT.MakeActive(as, "tomenuFilter")
    ELSE
      FormsVBT.MakeDormant(as, "tomenuFilter")
    END;
    FormsVBT.PutInteger(as, "vstretch", nv.StretchY);
    FormsVBT.PutInteger(as, "vshrink", nv.ShrinkY);
    FormsVBT.PutInteger(as, "hstretch", nv.StretchX);
    FormsVBT.PutInteger(as, "hshrink", nv.ShrinkX);
    FormsVBT.PutText(as, "tfgctypein", nv.TitleFgColor, FALSE);
    FormsVBT.PutText(as, "tbgctypein", nv.TitleBgColor, FALSE);
    FormsVBT.PutText(as, "ttyp", nv.TitleString, FALSE);
    FormsVBT.PutText(as, "supportCodeEditor", nv.SupportCode, FALSE);
    SplitLoadAttributes(nv, as); (* call split attributes *)
    DialogMenu.LoadAttributes(nv);
  END FormLoadAttributes;

PROCEDURE FormApplyAttributes (nv: FormNode; as: FormsVBT.T) =
  BEGIN
    SplitApplyAttributes(nv, as);
    IF FormsVBT.GetBoolean(as, "anchored") THEN
      WITH name       = FormsVBT.GetText(as, "parent"),
           classindex = NameToIndex("form")             DO
        nv.ParentForm := GetNodeNamed(name, classindex);
      END
    ELSE
      nv.ParentForm := NIL;
    END;
    nv.StretchY := FormsVBT.GetInteger(as, "vstretch");
    nv.ShrinkY := FormsVBT.GetInteger(as, "vshrink");
    nv.StretchX := FormsVBT.GetInteger(as, "hstretch");
    nv.ShrinkX := FormsVBT.GetInteger(as, "hshrink");
    nv.SupportCode := FormsVBT.GetText(as, "supportCodeEditor");
    nv.TitleFgColor := FormsVBT.GetText(as, "tfgctypein");
    IF Text.Equal(nv.TitleFgColor, "Inherit") THEN
      nv.TitleFgColor := nv.FgColor;
      FormsVBT.PutText(as, "tfgctypein", nv.TitleFgColor, FALSE);
    END;

    nv.TitleBgColor := FormsVBT.GetText(as, "tbgctypein");
    IF Text.Equal(nv.TitleBgColor, "Inherit") THEN
      nv.TitleBgColor := nv.BgColor;
      FormsVBT.PutText(as, "tbgctypein", nv.TitleBgColor, FALSE);
    END;

    nv.TitleString := FormsVBT.GetText(as, "ttyp");
    nv.HasMenu := FormsVBT.GetBoolean(as, "menuBoolean");

  END FormApplyAttributes;


PROCEDURE FormComputeSX (nv: FormNode; Final: BOOLEAN := FALSE): TEXT =
  BEGIN
    IF NOT Text.Equal(nv.InitialState, "Vanish") THEN
      nv.DialogSX := FindAndReplace(nv.DialogSX, "ZChildState", "Open");
    ELSE
      nv.DialogSX := FindAndReplace(nv.DialogSX, "ZChildState", "");
    END;
    nv.DialogSX :=
      FindAndReplace(nv.DialogSX, "XStretch", Fmt.Int(nv.StretchX));
    nv.DialogSX :=
      FindAndReplace(nv.DialogSX, "YStretch", Fmt.Int(nv.StretchY));
    nv.DialogSX :=
      FindAndReplace(nv.DialogSX, "XShrink", Fmt.Int(nv.ShrinkX));
    nv.DialogSX :=
      FindAndReplace(nv.DialogSX, "YShrink", Fmt.Int(nv.ShrinkY));
    IF nv.HasMenu THEN
      nv.DialogSX := FindAndReplace(nv.DialogSX, "MenuStructure",
                                    DialogMenu.ComputeMenuSX(nv));
    ELSE
      nv.DialogSX := FindAndReplace(nv.DialogSX, "MenuStructure", "");
    END;
    nv.DialogSX :=
      FindAndReplace(nv.DialogSX, "TitleBgColor", nv.TitleBgColor);
    nv.DialogSX :=
      FindAndReplace(nv.DialogSX, "TitleFgColor", nv.TitleFgColor);
    nv.DialogSX :=
      FindAndReplace(nv.DialogSX, "TitleString", nv.TitleString);
    (* print("After FormComputeSX :\n" & nv.DialogSX & "\n"); *)
    RETURN SplitComputeSX(nv, Final);
    (* after which there should be no unresolved symbols *)
  END FormComputeSX;

PROCEDURE FormGenerateObjectDefs (nv: FormNode): TEXT =
  VAR objdefs := "";
  BEGIN
    (* for all anchored forms & menu items within, generate obj defs *)

    (* anchored forms *)
    FOR j := 0 TO nv.NoOfChildren - 1 DO
      objdefs := objdefs & nv.ChildForms[j].generateObjectDefs()
    END;

    (* for all menu items generate menu item objdefs *)
    IF nv.HasMenu THEN
      objdefs := objdefs & DialogMenu.ComputeMenuObjDefs(nv);
    END;

    RETURN objdefs & SplitGenerateObjectDefs(nv);
  END FormGenerateObjectDefs;

PROCEDURE FormGenerateCallbacks (nv: FormNode): TEXT =
  VAR cbdefs := "";
  BEGIN

    (* for all anchored forms & menu items within, generate callbacks *)

    (* anchored forms *)
    FOR j := 0 TO nv.NoOfChildren - 1 DO
      cbdefs := cbdefs & nv.ChildForms[j].generateCallbacks()
    END;

    (* For all menu items generate menu item callbacks *)
    IF nv.HasMenu THEN
      cbdefs := cbdefs & DialogMenu.ComputeMenuCallbacks(nv);
    END;

    RETURN cbdefs & SplitGenerateCallbacks(nv);

  END FormGenerateCallbacks;

PROCEDURE FormGenerateAttachments (nv: FormNode): TEXT =
  VAR adefs := "";
  BEGIN
    (* forms and frames have no attachments but their components may *)

    (* for all anchored forms & menu items within, generate attachments *)
    (* anchored forms *)
    FOR j := 0 TO nv.NoOfChildren - 1 DO
      adefs := adefs & nv.ChildForms[j].generateAttachments()
    END;

    (* For all menu items generate menu item attachments *)
    IF nv.HasMenu THEN
      adefs := adefs & DialogMenu.ComputeMenuAttachments(nv)
    END;

    RETURN adefs & SplitGenerateAttachments(nv);

  END FormGenerateAttachments;

PROCEDURE FormGenerateInitializationCode (nv: FormNode): TEXT =
  VAR prefix := "";
  BEGIN
    (* for all anchored forms & menu items within, gen init code *)
    (* anchored forms *)
    FOR j := 0 TO nv.NoOfChildren - 1 DO
      prefix := prefix & nv.ChildForms[j].generateInitializationCode()
    END;

    RETURN prefix & SplitGenerateInitializationCode(nv);
  END FormGenerateInitializationCode;

(************* Frame Attribute Management *******************)



(************  Support Code **********************************)


PROCEDURE FindAndReplace (string, quarry, replacement: TEXT;
                          delimiter                  : CHAR   := '$'):
  TEXT =
  VAR
    first: INTEGER := Text.FindChar(string, delimiter);
    next : INTEGER;
  BEGIN
    IF first = -1 THEN RETURN string; END;
    next := Text.FindChar(string, delimiter, first + 1);
    WHILE next # -1 DO
      IF Text.Equal(Text.Sub(string, first + 1, next - first - 1), quarry) THEN
        RETURN Text.Sub(string, 0, first) & replacement
                 & Text.Sub(string, next + 1);
      END;
      first := next;
      next := Text.FindChar(string, delimiter, first + 1);
    END;
    RETURN string;
  END FindAndReplace;

PROCEDURE FAndRAll (string, quarry, replacement: TEXT;
                    delimiter                  : CHAR   := '$'): TEXT =
  VAR
    first: INTEGER := Text.FindChar(string, delimiter);
    next : INTEGER;
  BEGIN
    IF first = -1 THEN RETURN string; END;
    next := Text.FindChar(string, delimiter, first + 1);
    WHILE next # -1 DO
      IF Text.Equal(Text.Sub(string, first + 1, next - first - 1), quarry) THEN
        RETURN Text.Sub(string, 0, first)
                 & FAndRAll(replacement & Text.Sub(string, next + 1),
                            quarry, replacement, delimiter);
      END;
      first := next;
      next := Text.FindChar(string, delimiter, first + 1);
    END;
    RETURN string;
  END FAndRAll;

PROCEDURE GetDomain (v: FormNode): Rect.T =
  VAR
    fv          := v.getchild();
    back: VBT.T;
    rect        := VBT.Domain(v);
  BEGIN

    IF ISTYPE(fv, FormsVBT.T) THEN
      (* which should always be the case *)
      back :=
        FormsVBT.GetVBT(NARROW(fv, FormsVBT.T), v.name & "background");
      rect := VBT.Domain(back);
    END;
    (* move  the e,w,s boundaries by 4 inward *)
    rect.east := rect.east-4;
    rect.west := rect.west+4;
    rect.south := rect.south-4;
    RETURN rect;
  END GetDomain;

PROCEDURE GetTitleHt( <* UNUSED *>f : FormNode) : INTEGER =
  BEGIN
    RETURN 20; (* This is currently a constant *)
  END GetTitleHt;

(******************  CLASS MANAGEMENT *****************************************)
PROCEDURE Register (className      : TEXT;
                    createProc     : Proc;
                    minParentWidth : INTEGER := 100;
                    minParentHeight: INTEGER := 100;
                    attrsheetName  : TEXT    := "Default"): CARDINAL
  =
  VAR classnumber := ClassCounter;
  BEGIN
    IF NOT inited THEN
      ObjectClasses := NEW(REF ARRAY [0 .. 100] OF ObjClass);
      inited := TRUE;
    END;
    (* assumes that this class has not been registered before *)
    WITH cl = ObjectClasses[ClassCounter] DO
      cl.name := className;
      cl.createProc := createProc;
      cl.minParentWidth := minParentWidth;
      cl.minParentHeight := minParentHeight;
      IF Text.Equal(attrsheetName, "Default") THEN
        cl.attrsheetName := className & "att"
      ELSE
        cl.attrsheetName := attrsheetName
      END;
      INC(ClassCounter);
      TRY
        WITH rd = Rsrc.Open(className & "TEMPLATE.fv", Dialog.rsrcPath) DO
          cl.SXTemplate := Rd.GetText(rd, LAST(CARDINAL));
          Rd.Close(rd)
        END
      EXCEPT
      | Rsrc.NotFound =>
            print("Not Found: " & className & "TEMPLATE.fv");
      END (* TRY *);
    END;
    RETURN classnumber;
  END Register;

PROCEDURE ReloadSExpressions() =
  BEGIN
    FOR i := 0 TO ClassCounter - 1 DO
      TRY
        WITH rd = Rsrc.Open( ObjectClasses[i].name & "TEMPLATE.fv", Dialog.rsrcPath) DO
          ObjectClasses[i].SXTemplate := Rd.GetText(rd, LAST(CARDINAL));
          Rd.Close(rd)
        END
      EXCEPT    
      | Rsrc.NotFound =>
            print("Not Found: " & ObjectClasses[i].name & "TEMPLATE.fv");
      END (* TRY *);
    END;
  END ReloadSExpressions;

PROCEDURE NameToIndex (className: TEXT): CARDINAL
  RAISES {InvalidObjectName} =
  BEGIN
    FOR i := 0 TO ClassCounter - 1 DO
      IF Text.Equal(className, ObjectClasses[i].name) THEN RETURN i END
    END;
    RAISE InvalidObjectName(className)
  END NameToIndex;

PROCEDURE NewObject (dialogFV : FormsVBT.T;
                     className: TEXT;
                     parent   : T          := NIL): T
  RAISES {InstanceListFull} =
  VAR
    instanceName: TEXT;
    instance    : T;
    found                  := FALSE;
    n           : CARDINAL;
  BEGIN

    TRY
      n := NameToIndex(className);
    EXCEPT
    | InvalidObjectName (foo) =>
        Dialog.message(
          dialogFV, "The class " & foo & " has not yet been implemented");
        RETURN NIL;
    END;

    WITH cl = ObjectClasses[n] DO
      INC(cl.instances);
      INC(cl.count);
      instanceName := cl.name & Fmt.Int(cl.count);
      (* Default names are of the form : foo1, foo2 .. *)
      FOR i := 0 TO 100 DO
        IF cl.instanceList[i] = NIL AND NOT found THEN (* found a free
                                                          entry*)
          found := TRUE;
          instance := cl.createProc();
          instance.parent := parent;
          instance.name := instanceName;
          instance.classIndex := n;


          instance.DialogSX := instance.SXTemplate();
          EVAL instance.computeSX();

          cl.instanceList[i] := instance;

          RETURN instance
        END
      END;
      RAISE InstanceListFull(className)
    END
  END NewObject;

PROCEDURE InsertObject (nv : T) 
    RAISES {InstanceListFull} =
  VAR
    classname := GetNodeTypeName(nv);
    classindex := NameToIndex(classname);
    found                  := FALSE;
  BEGIN
    WITH cl = ObjectClasses[classindex] DO
      INC(cl.instances);
      FOR i := 0 TO 100 DO
        IF cl.instanceList[i] = NIL AND NOT found THEN (* found a free
                                                          entry*)
          found := TRUE;
          cl.instanceList[i] := nv;
          RETURN;
        END
      END;
      RAISE InstanceListFull(classname)
    END
  END InsertObject;

PROCEDURE GetMinParentDimensions (    className      : TEXT;
                                  VAR minParentWidth : INTEGER;
                                  VAR minParentHeight: INTEGER  )
  RAISES {InvalidObjectName} =
  BEGIN
    WITH n  = NameToIndex(className),
         cl = ObjectClasses[n]        DO
      minParentWidth := cl.minParentWidth;
      minParentHeight := cl.minParentHeight;
    END
  END GetMinParentDimensions;

PROCEDURE GetNodeIndex (v: T): CARDINAL RAISES {InvalidNode} =
  BEGIN
    WITH ci    = v.classIndex,
         ilist = ObjectClasses[ci].instanceList DO
      FOR i := 0 TO 100 DO IF ilist[i] = v THEN RETURN i END END
    END;
    RAISE InvalidNode;
  END GetNodeIndex;

PROCEDURE NoOfClasses (): CARDINAL =
  BEGIN
    RETURN ClassCounter
  END NoOfClasses;

PROCEDURE GetNodeTypeName (v: T): TEXT =
  BEGIN
    RETURN ObjectClasses[v.classIndex].name;
  END GetNodeTypeName;

PROCEDURE GetAttributeSheetName (v: T): TEXT =
  BEGIN
    RETURN ObjectClasses[v.classIndex].attrsheetName;
  END GetAttributeSheetName;

PROCEDURE DeleteObject (obj: T) =
  BEGIN
    WITH n     = obj.classIndex,
         ilist = ObjectClasses[n].instanceList DO
      DEC(ObjectClasses[n].instances);
      FOR i := 0 TO 100 DO IF ilist[i] = obj THEN ilist[i] := NIL END END
    END
  END DeleteObject;

PROCEDURE IndexToName (classIndex: CARDINAL): TEXT =
  BEGIN
    RETURN ObjectClasses[classIndex].name;
  END IndexToName;

PROCEDURE NoOfObjects (index: CARDINAL): CARDINAL =
  BEGIN
    RETURN ObjectClasses[index].instances;
  END NoOfObjects;

PROCEDURE GetFirst (classIndex: CARDINAL): T =
  BEGIN
    WITH ilist = ObjectClasses[classIndex].instanceList DO
      FOR i := 0 TO 100 DO
        IF ilist[i] # NIL THEN
          ObjectClasses[classIndex].last := i;
          RETURN ilist[i]
        END
      END
    END;
    RETURN NIL
  END GetFirst;

PROCEDURE GetNext (classIndex: CARDINAL): T =
  BEGIN
    WITH oc    = ObjectClasses[classIndex],
         ilist = oc.instanceList            DO
      FOR i := oc.last + 1 TO 100 DO
        IF ilist[i] # NIL THEN oc.last := i; RETURN ilist[i] END
      END
    END;
    RETURN NIL
  END GetNext;

PROCEDURE GetNodeNamed (name: TEXT; classIndex: INTEGER := -1): T =
  VAR start, end: CARDINAL;
  BEGIN
    IF classIndex = -1 THEN
      start := 0;
      end := ClassCounter - 1;
    ELSE
      start := classIndex;
      end := classIndex
    END;
    FOR i := start TO end DO
      WITH oc = ObjectClasses[i] DO
        FOR j := 0 TO 100 DO
          IF oc.instanceList[j] # NIL THEN
            IF Text.Equal(oc.instanceList[j].name, name) THEN
              RETURN oc.instanceList[j]
            END
          END
        END
      END
    END;
    RETURN NIL
  END GetNodeNamed;

PROCEDURE FormConstructor (): T =
  BEGIN
    RETURN NEW(FormNode, BgColor := "Grey75", FgColor := "Black", Rim := 0,
               Border := 1, Font := "-*-helvetica-bold-*R-*120-*",
               width := 100, height := 100, Embellishment := "Raised");
  END FormConstructor;

PROCEDURE FrameConstructor (): T =
  BEGIN
    RETURN NEW(FrameNode, BgColor := "Grey75", FgColor := "Black",
               Rim := 0, Border := 0, Embellishment := "Raised");
  END FrameConstructor;

PROCEDURE AllWhitespace (t: TEXT): BOOLEAN =
  VAR reader: Rd.T;
  BEGIN
    IF Text.Empty(t) THEN RETURN TRUE; END;
    reader := TextRd.New(t);
    Lex.Skip(reader);
    RETURN Rd.EOF(reader);
  END AllWhitespace;

(* save and load procs *)

PROCEDURE SaveToFile (fv: FormsVBT.T; s: Wr.T) =
  VAR current: T;
  BEGIN
    RW.wint(s, ClassCounter);    (* no of classes that follow *)
    FOR i := 0 TO ClassCounter - 1 DO
      (* write class description to file *)
      RW.wtext(s, ObjectClasses[i].name);
      RW.wint(s, ObjectClasses[i].instances);
      RW.wint(s, ObjectClasses[i].count);
      RW.wtext(s, ObjectClasses[i].SXTemplate);
      FOR j := 1 TO ObjectClasses[i].instances DO
        IF j = 1 THEN
          current := GetFirst(i);
        ELSE
          current := GetNext(i)
        END;
        current.save(fv, s);
      END;
    END;

  END SaveToFile;

PROCEDURE ResetTables () =
  BEGIN
    FOR i := 0 TO ClassCounter - 1 DO
      FOR j := 0 TO 100 DO ObjectClasses[i].instanceList[j] := NIL; END;
      ObjectClasses[i].instances := 0;
      ObjectClasses[i].count := 0;
    END;
  END ResetTables;

PROCEDURE LoadFromFile (fv: FormsVBT.T; s: Rd.T) =
  VAR
    classCount, cx: INTEGER;
    cname         : TEXT;

  BEGIN
    (* clean up *)
    GenerateObliq.sessionConstructor := "CreateEachFormOnce(LOCAL);\n";
    GenerateObliq.globalCode  := "";
    GenerateObliq.serverSideCode  := "";

    (* all screens have been deleted & fresh screens have been created *)
    
    (* We want to keep the ObjectClasses as they are but their
       instanceLists need to be freed *)
    print("Cleaning up \n");

    FOR i := 0 TO ClassCounter - 1 DO
      FOR j := 0 TO 100 DO ObjectClasses[i].instanceList[j] := NIL; END
    END;
    
    RW.rint(s, classCount);      (* no of classes that follow *)
    
    FOR i := 1 TO classCount DO
      (* read class description from file *)
      RW.rtext(s, cname);
     
      TRY
        cx := NameToIndex(cname)
      EXCEPT
      ELSE
        Dialog.message(
          Dialog.screen[1], "Unknown class specification - Load Aborted ")
      END;
      RW.rint(s, ObjectClasses[cx].instances);
      RW.rint(s, ObjectClasses[cx].count);
      RW.rtext(s, ObjectClasses[cx].SXTemplate);
    
      FOR j := 0 TO ObjectClasses[cx].instances - 1 DO
        WITH instance = ObjectClasses[cx].createProc() DO
          ObjectClasses[cx].instanceList[j] := instance;
          instance.load(fv, s);
          instance.classIndex := cx;
        END
      END;
    END;
    IF  NOT  FormsVBT.GetBoolean(fv, "useSSX") THEN
      ReloadSExpressions();
    END;
    print("Done\n");
    (* Resolve References - T.partuple, SplitNode.childtuples and
       FormNode.Partuple *)

    FOR i := 0 TO ClassCounter - 1 DO
      FOR j := 0 TO ObjectClasses[i].instances - 1 DO
        WITH current = ObjectClasses[i].instanceList[j] DO
          print("Computing sx for " & current.name & "\n");
          (* compute the sx while we're about it *)
          current.DialogSX := current.SXTemplate();
          EVAL current.computeSX();

          current.parent := RW.ttop(current.partuple);
          IF ISTYPE(current, SplitNode) THEN
            WITH sn = NARROW(current, SplitNode) DO
              FOR k := 1 TO sn.nc DO
                sn.children[k] := RW.ttop(sn.childtuples[k]);
              END
            END;
            IF ISTYPE(current, FormNode) THEN
              WITH fn = NARROW(current, FormNode) DO
                fn.ParentForm := NARROW(RW.ttop(fn.Partuple), FormNode);
              END
            END
          END
        END
      END
    END;

    (* insert FormNodes into appropriate screens *)
    WITH fx = NameToIndex("form") DO
      FOR j := 0 TO ObjectClasses[fx].instances - 1 DO
        WITH cur     = ObjectClasses[fx].instanceList[j],
             fn      = NARROW(cur, FormNode),
             dialog  = Dialog.screen[fn.Screen],
             zsplit  = FormsVBT.GetVBT(dialog, "topZSplit"),
             newform = NEW(FormsVBT.T).init(cur.DialogSX),
             x       = ROUND(Dialog.PixelsPerPtHor * FLOAT(cur.x)),
             y       = ROUND(Dialog.PixelsPerPtVer * FLOAT(cur.y)),
             width   = ROUND(Dialog.PixelsPerPtHor * FLOAT(cur.width)),
             height  = ROUND(Dialog.PixelsPerPtVer * FLOAT(cur.height)) DO
          EVAL ZHandleVBT.T.init(cur, newform, dialog.selection);
          ZSplit.InsertAt(NARROW(zsplit, ZSplit.T), cur,
                          (* Point.Add(Rect.NorthWest(VBT.Domain(zsplit)),
                             Point.T{50 + j, 50 + j}) *)
                          Point.T{x, y});
          ZSplit.Move(
            cur,
            Rect.FromCorners(Point.T{x, y}, Point.T{x + width, y + height}));
          RecursivelyInsertChildren(fn, dialog);
        END
      END
    END

  END LoadFromFile;


(* x = ROUND(Pts.ToPixels(nv, FLOAT(cur.x), Axis.T.Hor)), y =
   ROUND(Pts.ToPixels(nv, FLOAT(cur.y), Axis.T.Ver)), wid =
   ROUND(Pts.ToPixels(nv, FLOAT(cur.width), Axis.T.Hor)), ht =
   ROUND(Pts.ToPixels(nv, FLOAT(cur.height), Axis.T.Ver)) *)

PROCEDURE RecursivelyDeleteFromTables (csn: T) =
  BEGIN
    IF ISTYPE(csn, SplitNode) THEN
      WITH s = NARROW(csn, SplitNode) DO
        FOR i := 1 TO s.nc DO RecursivelyDeleteFromTables(s.children[i]) END
      END
    END;
    DeleteObject(csn);
  END RecursivelyDeleteFromTables;






(* x = ROUND(Pts.ToPixels(nv, FLOAT(cur.x), Axis.T.Hor)), y =
   ROUND(Pts.ToPixels(nv, FLOAT(cur.y), Axis.T.Ver)), wid =
   ROUND(Pts.ToPixels(nv, FLOAT(cur.width), Axis.T.Hor)), ht =
   ROUND(Pts.ToPixels(nv, FLOAT(cur.height), Axis.T.Ver)) *)

PROCEDURE RecursivelyInsertInTables (csn: T; se: ZHandleVBT.Selection) =
  <* FATAL InstanceListFull *>
  BEGIN
    IF ISTYPE(csn, SplitNode) THEN
      WITH s = NARROW(csn, SplitNode) DO
        FOR i := 1 TO s.nc DO RecursivelyInsertInTables(s.children[i], se) END
      END
    END;
    InsertObject(csn);
    ZHandleVBT.NewSelection(csn, se);
  END RecursivelyInsertInTables;


PROCEDURE RecursivelyInsertChildren (nv: SplitNode; dialog: Dialog.T) =
  BEGIN
    (* assert : nv has already been inserted *)
    FOR i := 1 TO nv.nc DO
      WITH cur     = nv.children[i],
           newform = NEW(FormsVBT.T).init(cur.DialogSX),
           x       = ROUND(Dialog.PixelsPerPtHor * FLOAT(cur.x)),
           y       = ROUND(Dialog.PixelsPerPtVer * FLOAT(cur.y)),
           width   = ROUND(Dialog.PixelsPerPtHor * FLOAT(cur.width)),
           height  = ROUND(Dialog.PixelsPerPtVer * FLOAT(cur.height)) DO
        EVAL ZHandleVBT.T.init(cur, newform, dialog.selection);
        ZSplit.InsertAt(nv, cur, Point.T{x, y});
        print("Inserting " & cur.name & " at " & Fmt.Int(x) & ","
                & Fmt.Int(y) & "\n");

        ZSplit.Move(cur, Rect.FromCorners(
                           Point.T{x, y}, Point.T{x + width, y + height}));

        WITH dom = ZSplit.GetDomain(cur) DO
          print("Final Dimensions of " & cur.name & " = "
                  & Fmt.Int(Rect.HorSize(dom)) & " X "
                  & Fmt.Int(Rect.VerSize(dom)) & "\n");
        END;
        IF ISTYPE(cur, SplitNode) THEN
          RecursivelyInsertChildren(cur, dialog);
        END
      END
    END
  END RecursivelyInsertChildren;

PROCEDURE Save (nv: T; <* UNUSED *> fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    ComputeDimensions(nv);
    nv.partuple := RW.ptot(nv.parent);
    RW.wtuple(s, nv.partuple);
    RW.wtext(s, nv.name);
    RW.wint(s, nv.x);
    RW.wint(s, nv.y);
    RW.wint(s, nv.width);
    RW.wint(s, nv.height);
    print("Saving dimensions of " & nv.name & ": " & Fmt.Int(nv.x) & ","
            & Fmt.Int(nv.y) & "," & Fmt.Int(nv.width) & ","
            & Fmt.Int(nv.height) & "\n");
    RW.wtext(s, nv.BgColor);
    RW.wtext(s, nv.FgColor);
    RW.wtext(s, nv.Font);
    RW.wcard(s, nv.Rim);
    RW.wcard(s, nv.Border);
    RW.wtext(s, nv.Embellishment);
    RW.wtext(s, nv.InitialState);
    RW.wbool(s, nv.Foreground);
    RW.wbool(s, nv.Local);
    RW.wtext(s, nv.Location);
    RW.wtext(s, nv.Code);
    RW.wtext(s, nv.ResizeModel);
    (* classIndex may change *)
  END Save;

PROCEDURE Load (nv: T; <*UNUSED*>fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    RW.rtuple(s, nv.partuple);
    RW.rtext(s, nv.name);
    RW.rint(s, nv.x);
    RW.rint(s, nv.y);
    RW.rint(s, nv.width);
    RW.rint(s, nv.height);
    print("Loading dimensions of " & nv.name & ": " & Fmt.Int(nv.x) & ","
            & Fmt.Int(nv.y) & "," & Fmt.Int(nv.width) & ","
            & Fmt.Int(nv.height) & "\n");
    RW.rtext(s, nv.BgColor);
    RW.rtext(s, nv.FgColor);
    RW.rtext(s, nv.Font);
    RW.rcard(s, nv.Rim);
    RW.rcard(s, nv.Border);
    RW.rtext(s, nv.Embellishment);
    RW.rtext(s, nv.InitialState);
    RW.rbool(s, nv.Foreground);
    RW.rbool(s, nv.Local);
    RW.rtext(s, nv.Location);
    RW.rtext(s, nv.Code);
    RW.rtext(s, nv.ResizeModel);
    (* classIndex is set in LoadFromFile *)
    (* note parent hasn't been set yet - it is derived from nv.partuple *)
  END Load;

PROCEDURE SplitSave (nv: SplitNode; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    Save(nv, fv, s);
    RW.wcard(s, nv.nc);
    FOR i := 1 TO nv.nc DO RW.wtuple(s, RW.ptot(nv.children[i])) END;
  END SplitSave;

PROCEDURE SplitLoad (nv: SplitNode; fv: FormsVBT.T; s: Rd.T) =
  BEGIN
    Load(nv, fv, s);
    RW.rcard(s, nv.nc);
    FOR i := 1 TO nv.nc DO RW.rtuple(s, nv.childtuples[i]) END;
  END SplitLoad;

PROCEDURE FormSave (nv: FormNode; fv: FormsVBT.T; s: Wr.T) =
  BEGIN
    SplitSave(nv, fv, s);

    IF nv.HasMenu AND nv.Menu # NIL THEN
      RW.wbool(s, TRUE);
      RW.wtext(s, nv.MenuBgColor);
      RW.wtext(s, nv.MenuFgColor);
      RW.wtext(s, nv.MenuFont);
      RW.wcard(s, NUMBER(nv.Menu^));
      FOR i := FIRST(nv.Menu^) TO LAST(nv.Menu^) DO
        RW.wcard(s, nv.Menu[i].Level);
        RW.wtext(s, nv.Menu[i].Label);
        RW.wtext(s, nv.Menu[i].Name);
        RW.wbool(s, nv.Menu[i].inForeGround);
        RW.wbool(s, nv.Menu[i].isLocal);
        RW.wtext(s, nv.Menu[i].executeAt);
        RW.wtext(s, nv.Menu[i].initialState);
        RW.wtext(s, nv.Menu[i].callback);
      END;
    ELSE
      RW.wbool(s, FALSE)
    END;

    RW.wtuple(s, RW.ptot(nv.ParentForm));
    RW.wtext(s, nv.SupportCode);
    RW.wcard(s, nv.Screen);
    RW.wtext(s, nv.TitleBgColor);
    RW.wtext(s, nv.TitleFgColor);
    RW.wtext(s, nv.TitleString);
    RW.wcard(s, nv.StretchX);
    RW.wcard(s, nv.StretchY);
    RW.wcard(s, nv.ShrinkX);
    RW.wcard(s, nv.ShrinkY);
  END FormSave;

PROCEDURE FormLoad (nv: FormNode; fv: FormsVBT.T; s: Rd.T) =
  VAR menusize: CARDINAL;
  BEGIN
    SplitLoad(nv, fv, s);
    RW.rbool(s, nv.HasMenu);
    IF nv.HasMenu THEN
      RW.rtext(s, nv.MenuBgColor);
      RW.rtext(s, nv.MenuFgColor);
      RW.rtext(s, nv.MenuFont);
      RW.rcard(s, menusize);
      nv.Menu := NEW(REF ARRAY OF DialogMenu.T, menusize);
      FOR i := FIRST(nv.Menu^) TO LAST(nv.Menu^) DO
        RW.rcard(s, nv.Menu[i].Level);
        RW.rtext(s, nv.Menu[i].Label);
        RW.rtext(s, nv.Menu[i].Name);
        RW.rbool(s, nv.Menu[i].inForeGround);
        RW.rbool(s, nv.Menu[i].isLocal);
        RW.rtext(s, nv.Menu[i].executeAt);
        RW.rtext(s, nv.Menu[i].initialState);
        RW.rtext(s, nv.Menu[i].callback);
      END
    END;

    RW.rtuple(s, nv.Partuple);
    RW.rtext(s, nv.SupportCode);
    RW.rcard(s, nv.Screen);
    RW.rtext(s, nv.TitleBgColor);
    RW.rtext(s, nv.TitleFgColor);
    RW.rtext(s, nv.TitleString);
    RW.rcard(s, nv.StretchX);
    RW.rcard(s, nv.StretchY);
    RW.rcard(s, nv.ShrinkX);
    RW.rcard(s, nv.ShrinkY);
  END FormLoad;

PROCEDURE FormObAttrs(nv: FormNode) : TEXT =
  VAR ret := "";
  BEGIN
    (* need to put in support for form-anchoring *)
    (* not clear where.. *)

    IF nv.ParentForm # NIL THEN
      ret := ret & "\ttemp.ParentForm := meth(s) VOInstance." & nv.ParentForm.name &
                 " end;\n";
    END;

    IF nv.NoOfChildren > 0 THEN
      ret := ret & "\ttemp.ChildForms := meth(s)  [";
      FOR i := 0 TO nv.NoOfChildren-1 DO
        ret := ret & " VOInstance." & nv.ChildForms[i].name  & ",";
      END;
      ret := ret & "] end;\n";
    END;

   IF nv.HasMenu AND nv.Menu # NIL THEN
      ret := ret & BoolAttr("HasMenu", TRUE) &
      TextAttr("MenuBgColor", nv.MenuBgColor) &
      TextAttr("MenuFgColor",  nv.MenuFgColor) &
      TextAttr("MenuFont", nv.MenuFont);
      
      ret := ret & "\ttemp.Menu := meth(s)  [";
      FOR i := FIRST(nv.Menu^) TO LAST(nv.Menu^) DO
        IF nv.Menu[i].Level > 0 OR NOT (Text.Equal(nv.Menu[i].Name, "RIDGE")) THEN
          ret := ret & " VOInstance." & nv.Menu[i].Name  & ",";
        END;
      END;
      ret := ret & "] end;\n";
      
    ELSE
      ret := ret & BoolAttr("HasMenu", FALSE)
    END;
   
   ret := ret & TextAttr("TitleBgColor", nv.TitleBgColor) & 
              TextAttr("TitleFgColor", nv.TitleFgColor) &
              TextAttr("TitleString", nv.TitleString) &
              IntAttr("StretchX", nv.StretchX) &
              IntAttr("StretchY", nv.StretchY) &
              IntAttr("ShrinkX", nv.ShrinkX) &
              IntAttr("ShrinkY", nv.ShrinkY);
    
    RETURN SplitObAttrs(nv) & ret;
  END FormObAttrs; 
 
PROCEDURE GetInfo(topic:TEXT) : REF InfoDefn =
VAR 
  volib : Rd.T;
  delimiterFound : CHAR;
  s, e : CARDINAL;
  signature : TEXT;
(* Procedure that reads progressively down a file and finds
   comments of the form (* ... *). It then looks to see if there
   is delimiter from signset immediately  inside th comment.
   If so the contents minus any preceding/trailing blanks is
   returned and delimiterFound is set appropriately *)

PROCEDURE Find(c: CHAR) : BOOLEAN 
RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN  
    WITH got = Rd.GetChar(volib) DO
      IF got = c THEN
        RETURN TRUE;
      ELSE 
        Rd.UnGetChar(volib);
        RETURN FALSE;
      END (* IF *)
    END (* WITH *)
  END Find;

PROCEDURE ParseCommentStart() 
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    LOOP
      REPEAT
      UNTIL  Rd.GetChar(volib) = '(';
      IF Find('*') THEN RETURN;
      END (* IF *)
    END (* LOOP *)
  END ParseCommentStart;

PROCEDURE GetBodyOfComment():TEXT 
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR 
    current : CHAR;
    retval := "";
  BEGIN
    LOOP
      current := Rd.GetChar(volib);
      WHILE  current # '*' DO
        retval := retval & Text.FromChar(current);
        current := Rd.GetChar(volib);
      END;
      IF Find(')') THEN 
        RETURN retval;
      ELSE
        retval := retval & "*";
      END (* IF *)
    END (* LOOP *)
  END GetBodyOfComment;

PROCEDURE GetNextSignature(signset : SET OF CHAR): TEXT =
  VAR comment: TEXT;
      ret_val : TEXT;
  BEGIN
    TRY

      LOOP
        ParseCommentStart();
        comment := GetBodyOfComment();        
      
        (* is this a valid comment ? *)
        WITH 
          f = Text.GetChar(comment, 0),
          l = Text.GetChar(comment, Text.Length(comment) -1)
         DO
          IF f IN signset AND f=l THEN
           
            delimiterFound := f;
            ret_val :=  Text.Sub(comment, 1,  Text.Length(comment) -2);
            EXIT;
          END (* IF *)
        END (* WITH *)
      END (* LOOP *);

      EXCEPT 
      ELSE
        
        RETURN NIL;
      END (* TRY *);

    RETURN ret_val;

  END GetNextSignature;

 
BEGIN
   print("Getting Info on " & topic & "\n");
    IF NOT createdInfoList THEN
      (* index = 0 is for volib methods*)
      infoList[infoCtr] := NEW(REF InfoDefn, topic:="Local",
                              info:= "\t\tMethods in the Visual Obliq Library\n"
                                   & "\t\t***********************************\n\n");
      INC(infoCtr);

      (* open internal volib.obl *)
      TRY
        volib := Rsrc.Open("volib.obl", Dialog.rsrcPath);
     
      LOOP
        WITH sig = GetNextSignature(SET OF CHAR{'=',':','-'}) DO
       
          IF sig = NIL THEN
            EXIT;
          ELSE
      
            (* strip initial and final blanks *)
            s:= 0;    e := Text.Length(sig)-1;
            WHILE s<= e DO 
              IF NOT Text.GetChar(sig, s) IN  ASCII.Spaces THEN
                EXIT;
              ELSE
                INC(s); 
              END;
            END;  
            
            WHILE e > s DO
              IF NOT Text.GetChar(sig, e) IN  ASCII.Spaces THEN
                EXIT;
              ELSE
                DEC(e); 
              END;
            END;
            (* s and e delimit the actual string *)
            signature := Text.Sub(sig, s, e-s+1);
            
            CASE delimiterFound OF
              | '=' =>     
                  infoList[infoCtr] := NEW(REF InfoDefn, topic:= signature,
                  info:="\t\tMethods for " & signature &  "\n\n");
                    INC(infoCtr);
            
              | '-' =>
                  infoList[infoCtr-1].info := infoList[infoCtr-1].info & signature & "\n";

              | ':' =>
                  infoList[0].info := infoList[0].info & signature & "\n";
              ELSE
              END (* CASE *)
          END (* IF *)
        END (* WITH *) 
      END (* LOOP *);
      Rd.Close(volib);
      createdInfoList := TRUE;

      EXCEPT ELSE 
      END;      
    END (* IF *);

    (* Lookup topic and return a reference to the  information record  *)
    FOR i:=0 TO infoCtr-1 DO
      IF Text.Equal(infoList[i].topic, topic) THEN  
        RETURN infoList[i];
      END (* IF *)
    END (* FOR *);
  
    RETURN NIL;
END GetInfo;

PROCEDURE Initialize () =
  BEGIN
    EVAL Register("form", FormConstructor);
    EVAL Register("frame", FrameConstructor);

    WITH menuclosure = NEW(FormsVBT.Closure, apply := MenuProc) DO
      (* attach menu design button and menu boolean *)
      FormsVBT.Attach(Attributes.afv, "menuBoolean", menuclosure);
      FormsVBT.Attach(Attributes.afv, "tomenu", menuclosure);
    END;

    WITH lrclosure = NEW(FormsVBT.Closure, apply := LRProc) DO
      (* attach local and remote choices *)
      FormsVBT.Attach(Attributes.afv, "Local", lrclosure);
      FormsVBT.Attach(Attributes.afv, "Remote", lrclosure);
    END;

    WITH colorclosure = NEW(
                          FormsVBT.Closure, apply := Attributes.ColorProc),
         anchorclosure = NEW(FormsVBT.Closure, apply := AnchorProc),
         parclosure    = NEW(FormsVBT.Closure, apply := ParentProc)  DO
      (* attach form-attribute sheet color-popup-helper-buttons *)
      FormsVBT.Attach(Attributes.afv, "tbgc", colorclosure);
      FormsVBT.Attach(Attributes.afv, "tfgc", colorclosure);

      (* attach anchor enable boolean *)
      FormsVBT.Attach(Attributes.afv, "anchored", anchorclosure);
      (* attach browser selection *)
      FormsVBT.Attach(Attributes.afv, "parentlist", parclosure);

    END;
  END Initialize;



BEGIN
END NodeVBT.
