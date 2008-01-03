%interface %module %public %private %overrides
~\
INTERFACE %name;
%gen
(* extended parser definition *)
%import\
%interface\
TYPE
%gnTypes\

  T <: Public;
  Public = %meth.T OBJECT
%public\
  END;

  (* And now, for a hack to allow compatible methods *)
  (* ... without importing the original parser *)
  Original_Parser = %meth.Original_Parser;
%orig\
  (* ... and without importing the original token *)
%tokOrig\
%tkimp\
END %name.
~\\
PROCEDURE Proc_%name(self: T;
 VAR p0: Original_%return%oparams) =
  VAR
    result: %return;
%narrow\\
  BEGIN
    IF p0 = NIL THEN
      p0 := NewPT(self.allocate_%return, TYPECODE(%return));
    END;
    result := NARROW(p0, %return);(*%TYPEINIT%%return%*)
    %yaccName.T.%name(self, p0%cparams);
    result := NARROW(p0, %return);
    BEGIN (* user code *)
      %body
    END;
    p0 := result;
  END Proc_%name;

~\
MODULE %name;
%gen
IMPORT %meth;
%module\

REVEAL
  T = Public BRANDED "%name" OBJECT
%alloc\
%private\
  OVERRIDES
    purge := Proc_Purge;
%overrides\
%ovr\
  END;

PROCEDURE Proc_Purge(self: T): INTEGER =
  BEGIN
    RETURN %meth.T.purge(self)%purge;
  END Proc_Purge;

(* rule procedures *)
%gnProcs\
BEGIN
END %name.
