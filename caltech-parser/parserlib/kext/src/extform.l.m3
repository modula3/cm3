%interface %module %public %private %overrides
~\
INTERFACE %name;
%gen
(* extended lexer definition *)
%import\
%interface\
TYPE
%gnTypes\

  T <: Public;
  Public = %meth.T OBJECT
%public\
  END;
%tkimp\

(* generics stuff *)
CONST
  Brand = "%name";
  Hash = %meth.Hash;
  Equal = %meth.Equal;
END %name.
~\
PROCEDURE Proc_%name(self: T): Token =
  BEGIN (* user code *)
   %body
  END Proc_%name;

~\
MODULE %name;
%gen
IMPORT %meth;
%module\

REVEAL
  T = Public BRANDED Brand OBJECT
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

(* expression procedures *)
%gnProcs\
BEGIN
END %name.
