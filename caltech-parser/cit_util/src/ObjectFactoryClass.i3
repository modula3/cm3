(* $Id$ *)

INTERFACE ObjectFactoryClass;
IMPORT ObjectFactory;
IMPORT RTType;

REVEAL
  ObjectFactory.T <: Private;

TYPE
  Private = ObjectFactory.Public OBJECT
    code : RTType.Typecode
  END;

END ObjectFactoryClass.
