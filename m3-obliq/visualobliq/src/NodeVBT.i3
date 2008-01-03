(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Sat Sep 10 14:16:58 PDT 1994 by bharat *)
(*      modified on Wed Nov 17 16:06:30 PST 1993 by mhb    *)
<* PRAGMA LL *>


(* All components (forms/frames/widgets) in "NodeVBT" are instances *)
(* of NodeVBT *)

INTERFACE NodeVBT;

IMPORT DialogMenu, FormsVBT, Rd, Wr, ZHandleVBT;

EXCEPTION
  InvalidObjectName(TEXT);
  InstanceListFull(TEXT);
  InvalidNode;

TYPE
  Tuple = RECORD class : TEXT; name: TEXT; END;
  (* This is a storage format for pointers. class = "NIL" represents a nil
   pointer *)

  T <: Public;
  Public =
    ZHandleVBT.T OBJECT
      parent       : T;
      partuple     : Tuple;

      name         : TEXT;
      x, y         : INTEGER;    (* computed when needed *)
      width        : INTEGER   := 50; (* computed when needed *)
      height       : INTEGER   := 50; (* computed when needed *)
      BgColor      : TEXT      := "LightYellow";
      FgColor      : TEXT      := "Black";
      Font         : TEXT      := "Fixed";
      Rim          : CARDINAL  := 1;
      Border       : CARDINAL  := 1;
      Embellishment: TEXT      := "Flat";
      (* possible values are "Flat", "Raised", "Lowered", "Chiseled" ,
         "Ridged" and "None" *)
      InitialState: TEXT := "Active";
      (* possible values are "Active", "Passive", "Dormant", "Vanish" *)
      Foreground: BOOLEAN := TRUE;
      Local     : BOOLEAN := TRUE;
      Location  : TEXT    := "";
      
      DialogSX: TEXT;            (* current SX expression by customizing
                                    SXTemplate *)
      Code  : TEXT := "";
      (* This is Callback code for widgets and Initialization code
         for splitnodes *)
      ResizeModel : TEXT := "Scaled";
      (* possible values are "CenterPin", "Scaled", "HScaled", "VScaled" *)
      classIndex: CARDINAL;
    METHODS
      <* LL <= VBT.mu *>

      loadAttributes (v: FormsVBT.T);
      (* load attributes into the attribute sheet *)
      (* you will override this method to add attribute sheet
         initialization code *)
      (* be sure to call NodeVBT.T.loadAttributes within your method *)

      checkAttributes (as: FormsVBT.T; VAR error: TEXT): BOOLEAN;
      (* checks the validity of all the attributes entered by the user *)
      (* returns FALSE if there is an error and sets the error message *)
      (* you will override this method to add code to check the validity of
         your fields in the attribute sheet *)
      (* be sure to call NodeVBT.T.checkAttributes if you do not have an
         error *)


      applyAttributes (as: FormsVBT.T);
      (* make the changes effective *)
      (* you will override this in order to make changes to the attributes
         specific to your widget *)
      (* When you override this be sure to FIRST call the
         NodeVBT.applyAttributes method *)

      computeSX (Final: BOOLEAN := FALSE): TEXT;
      (* this uses the current attributes to customize SXTemplate giving
         the current s-expression, which is stored in DialogSX - this is
         also returned.  If Final is set then it generates the
         s-expressions for contained objects and anchored forms as well and
         is recursive. *)


      generateObjectDefs (): TEXT;
      (* generates code for the objects representing the widgets
         forms collect object defs for all components & menus & attached forms
         frames collect object defs for all components
         typically one def  per widget unless overridden  *)
      generateCallbacks (): TEXT;
      (* forms and frames have not callbacks but others do
         forms collect callbacks for all components & menus & attached forms
         frames collect callbacks for all components
         typically one callback per widget unless overridden *)
      generateAttachments() : TEXT;
      (* generates the formsvbt attachment
        typically one per widget unless overridden e.g. filebrowser
        forms collect attachments for all components & menus & attached forms
       frames collect attachments for all components *)
      generateInitializationCode() : TEXT;
      (* forms and frames have intialization code
         the order of initialization is depth first *)
    
      (* These 4 methods are invoked on a FormNode which may in turn invoke
         methods in components *)
      SXTemplate() : TEXT;         (* returns the template for this class *)
      save(fv : FormsVBT.T;  s : Wr.T);
      load(fv : FormsVBT.T; s: Rd.T);

      initObliqAttrs() : TEXT; (* generates the obliq widget constructor argument-list *)
     
    END;




  Widget <: PublicWidget;
  PublicWidget = T OBJECT END;

  SplitNode <: PublicSplit;

  PublicSplit = T OBJECT
                  children: ARRAY [1 .. 50] OF T;  
                  (* anchored forms do not  count as children *)
                  (* the array is compacted so that 1..nc is filled *)

                  childtuples : ARRAY[1..50] OF Tuple; 
                  (* computed when needed *)

                  nc: CARDINAL := 0;
                END;

  FormNode <: PublicForm;
  PublicForm =
    SplitNode OBJECT
      HasMenu    : BOOLEAN                   := FALSE;
      MenuBgColor: TEXT                      := "VeryPaleYellow";
      MenuFgColor: TEXT                      := "Black";
      MenuFont   : TEXT                      := "-*-helvetica-bold-*R-120-*";
      Menu       : REF ARRAY OF DialogMenu.T := NIL;

      
      ParentForm: FormNode := NIL;
      Partuple : Tuple;
      (* this is always up to date *)
      
      SupportCode : TEXT := ""; 
      (* may be used by all widgets within the form *)

      ChildForms: ARRAY [0 .. 50] OF FormNode;  (* computed when needed *)
      (* anchored forms - ChildForms is a  misnomer *)
      NoOfChildren: CARDINAL;    (* not guaranteed to be up to date *)

      Screen : CARDINAL;
    END;

  FrameNode <: PublicFrame;
  PublicFrame = SplitNode OBJECT END;
  Proc = PROCEDURE (): T;
  
  InfoDefn = RECORD
    topic: TEXT;
    info: TEXT;
  END;

VAR
  defaultBgColor        := "White";
  defaultFgColor        := "Black";
  defaultFont           := "Fixed";
  defaultName         := "Unnamed";
  blowEditingFont := FALSE;

PROCEDURE Register (className      : TEXT;
                    createProc     : Proc;
                    minParentWidth : INTEGER := 100;
                    minParentHeight: INTEGER := 100;
                    attrsheetName  : TEXT    := "Default"): CARDINAL;
(* This is used by various object classes to install their object types *)
(* An unique index is returned for each class that is registered *)
(* If attrsheetName is "Default" then className&"att" is used, otherwise *)
(* the user specified name is taken to be the name of the attr-sheet
   page *)

(* NOTE : If your widget class is called "foo" then your dialog editor
   widget should be called "foo.fv", your attributes sheet tsplit extension
   should normally be called "fooatt" and your widget menu entry will be
   called "FOO" *)
(* Pick a size that is at least 50 points larger than the default size of
   the widget, in each dimension *)
PROCEDURE NewObject (dialogFV : FormsVBT.T;
                     className: TEXT;
                     parent   : T            := NIL): T
  RAISES {InvalidObjectName, InstanceListFull};
(* This also instantiates the name field and loads the default
   s-expression *)

PROCEDURE GetMinParentDimensions (    className      : TEXT;
                                  VAR minParentWidth : INTEGER;
                                  VAR minParentHeight: INTEGER  )
  RAISES {InvalidObjectName};
(* minimum size of parent *)

PROCEDURE ReloadSExpressions();
(* Load S-expressions from files *)

PROCEDURE GetNodeIndex (v: T): CARDINAL RAISES {InvalidNode};
(* This returns a number that is unique for every node within a given
   class *)

PROCEDURE GetNodeTypeName (v: T): TEXT;
(* returns the name of the nodetype *)

PROCEDURE GetAttributeSheetName (v: T): TEXT;
(* returns name of the associated attribute sheet *)

PROCEDURE DeleteObject (obj: T);
(* This deletes it from the object manager database not from the dialog
   editor *)

PROCEDURE InsertObject (obj: T) RAISES {InstanceListFull};
(* This adds it to the object manager database not from the dialog
   editor *)

PROCEDURE NameToIndex (className: TEXT): CARDINAL
  RAISES {InvalidObjectName};

PROCEDURE IndexToName (classIndex: CARDINAL): TEXT;

PROCEDURE NoOfClasses (): CARDINAL;
PROCEDURE NoOfObjects (index: CARDINAL): CARDINAL;

PROCEDURE GetFirst (classIndex: CARDINAL): T;
(* First object of this class *)

PROCEDURE GetNext (classIndex: CARDINAL): T;
(* Next object of this class - Do not create or delete nodes between
   GetFirst and GetNext *)

PROCEDURE GetNodeNamed (name: TEXT; classIndex: INTEGER := -1): T;
(* if you do not specify classIndex it will search all classes *)
(* returns NIL if not present *)

(* To help customize attributes ... *)
PROCEDURE FindAndReplace (string, quarry, replacement: TEXT;
                          delimiter                  : CHAR   := '$'):
  TEXT;
(* Replaces the first occurrence of <delimiter><quarry><delimiter> with
   <replacement> *)

PROCEDURE FAndRAll (string, quarry, replacement: TEXT;
                     delimiter: CHAR   := '$'):TEXT;
(* recursive version that replaces all occurences *)
(* Warning - this includes the replacement as well - so it may never
   terminate if the replacement has the same quarry between the delimiters *)

PROCEDURE ComputeAnchoredFormTree ();
(* computes current values for *)

PROCEDURE ComputeDimensions (v: T);
(* computes current values for v.x v.y v.width and v.height *)

PROCEDURE AllWhitespace(t : TEXT) : BOOLEAN;

PROCEDURE SaveToFile(fv : FormsVBT.T; s : Wr.T);
PROCEDURE LoadFromFile(fv : FormsVBT.T; s : Rd.T);

PROCEDURE ResetTables();
(* clean up tables *)

PROCEDURE RecursivelyDeleteFromTables(csn : T);
(* delete tree of nodes from tables *)

PROCEDURE RecursivelyInsertInTables (csn: T; s : ZHandleVBT.Selection);
(* insert tree of nodes into table *)
(* doesn't set any fields ! *)
 
PROCEDURE print(c : TEXT);

PROCEDURE Initialize ();

PROCEDURE GetInfo(topic:TEXT) : REF InfoDefn;

(* Routines to generate Obliq code for initializing widget attributes *)
PROCEDURE IntAttr(name:TEXT; arg : INTEGER): TEXT;
PROCEDURE TextAttr(name:TEXT; arg :TEXT) : TEXT;
PROCEDURE BoolAttr(name:TEXT; arg :BOOLEAN) : TEXT;

END NodeVBT.





