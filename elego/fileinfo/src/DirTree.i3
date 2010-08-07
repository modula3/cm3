(*--------------------------------------------------------------------------*)
INTERFACE DirTree;

IMPORT Time;
IMPORT FileInfo, FindExpr, BoolSeq;
IMPORT APN AS APN, APNSeq AS APNSeq;

(*--------------------------------------------------------------------------*)
TYPE 
  SortCriterion = {
    None,
    FilesFirst,
    DirsFirst
  };

  LayoutClosure = PublicLayoutClosure;
  PublicLayoutClosure = BRANDED "DirTree.PublicLayoutClosure 0.0" OBJECT
  METHODS
    dir(pn : APN.T; 
        time : Time.T; size : LONGINT;
        level : CARDINAL; 
        lastPrefix : BoolSeq.T) : TEXT;
    file(pn : APN.T;
         time : Time.T; size : LONGINT;
         level : CARDINAL;
         lastPrefix : BoolSeq.T) : TEXT;
    sort(list : APNSeq.T) : APNSeq.T;
  END;

  SimpleTextLayout = LayoutClosure OBJECT
  METHODS
    fmtDir(pn : APN.T; time : Time.T; size : LONGINT) : TEXT := 
        SimpleTextFmtDir;
    fmtFile(pn : APN.T; time : Time.T; size : LONGINT) : TEXT := 
        SimpleTextFmtFile;
  (*
    dir(pn : APN.T; 
        time : Time.T; size : INTEGER;
        level : CARDINAL; 
        lastPrefix : BoolSeq.T) : TEXT := SimpleTextDirLayout;
    file(pn : APN.T;
         time : Time.T; size : INTEGER;
         level : CARDINAL;
         lastPrefix : BoolSeq.T) : TEXT := SimpleTextFileLayout;
    sort(list : APNSeq.T) : APNSeq.T := SimpleTextNoSort;
  *)
  OVERRIDES
    dir := SimpleTextDirLayout;
    file := SimpleTextFileLayout;
    sort := SimpleTextNoSort;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE SimpleTextDirLayout(self : SimpleTextLayout; pn : APN.T; 
                              time : Time.T; size : LONGINT;
                              level : CARDINAL; 
                              lastPrefix : BoolSeq.T) : TEXT;

(*--------------------------------------------------------------------------*)
PROCEDURE SimpleTextFileLayout(self : SimpleTextLayout; pn : APN.T; 
                               time : Time.T; size : LONGINT;
                               level : CARDINAL; 
                               lastPrefix : BoolSeq.T) : TEXT;

(*--------------------------------------------------------------------------*)
PROCEDURE SimpleTextFmtDir(self : SimpleTextLayout; 
                           pn : APN.T; time : Time.T; size : LONGINT) : TEXT;

(*--------------------------------------------------------------------------*)
PROCEDURE SimpleTextFmtFile(self : SimpleTextLayout; 
                            pn : APN.T; time : Time.T; size : LONGINT) : TEXT;

(*--------------------------------------------------------------------------*)
PROCEDURE SimpleTextNoSort(self : SimpleTextLayout; 
                           list : APNSeq.T) : APNSeq.T;

(*--------------------------------------------------------------------------*)
PROCEDURE Layout(cache   :  FileInfo.T;
                 root    :  APN.T;
                 lfuns   :  LayoutClosure := NIL; (* SimpleTextLayout *)
                 ignDir  :  FindExpr.T := NIL; 
                 ignFile :  FindExpr.T := NIL;
                 sc      := SortCriterion.None;
                 update  := FALSE) : TEXT;

VAR
  (* CONST *) simpleTextLayout : SimpleTextLayout;
END DirTree.
