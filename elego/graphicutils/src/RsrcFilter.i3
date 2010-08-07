(*--------------------------------------------------------------------------*)
INTERFACE RsrcFilter;

IMPORT Rd, Rsrc, Thread;

TYPE
  Closure = OBJECT
  METHODS
    apply(t : TEXT) : TEXT;
  END;

  T <: Public;

  Public = OBJECT
  METHODS
    (*----------------------------------------------------------------------*)
    init(p1, p2, p3, p4, p5, p6 := NIL) : T;
    (* Initializes the resource search path. *)

    (*----------------------------------------------------------------------*)
    addPath(p1, p2, p3, p4 : REFANY := NIL) : T;
    (* Append elements p1, p2, p3, p4 to the resource path, if they
       are not NIL.
    *)

    (*----------------------------------------------------------------------*)
    addFilter(f : Closure) : T;
    (* Adds a general filter that will be applied to every resource
       text obtained by getRsrc.
    *)

    (*----------------------------------------------------------------------*)
    addSubstFilter(s, t : TEXT) : T;
    (* Adds a filter that substitutes text `s' by text `t' in every
       resource obtained by getRsrc.
    *)

    (*----------------------------------------------------------------------*)
    getPath() : Rsrc.Path;
    (* Returns the current resource search path. *)

    (*----------------------------------------------------------------------*)
    getRsrc(name : TEXT) : TEXT 
    RAISES {Rsrc.NotFound, Rd.Failure, Thread.Alerted};
    (* Gets the named resource from the specified resource path and
       applies all defined filters before returning it.
    *)

    (*----------------------------------------------------------------------*)
    getRaw(name : TEXT) : TEXT 
    RAISES {Rsrc.NotFound, Rd.Failure, Thread.Alerted};
    (* Gets the named resource from the specified resource path 
       and returns it without applying any filters.
    *)

  END;


END RsrcFilter.
