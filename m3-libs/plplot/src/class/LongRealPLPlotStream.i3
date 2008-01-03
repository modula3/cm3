INTERFACE LongRealPLPlotStream;

IMPORT LongRealPLPlotFrame AS Frame, Pathname;

(* The routines are not thread safe, because PLPlot uses global variables.
   We did not add locking mechanisms. *)
TYPE
  T <: Public;
  Public = OBJECT
           METHODS
             put                   (frame: Frame.T; );
             setPenWidth           (width: CARDINAL; );
             setCharacterRelHeight (relHeight: LONGREAL; );
             setOrientation        (orient: [0 .. 3]; );

             exit ();
           END;

  Generic <: GenericPublic;
  GenericPublic = T OBJECT METHODS init (): Generic; END;

  XWindow <: XWindowPublic;
  XWindowPublic = T OBJECT METHODS init (): XWindow; END;

  PostScript <: PostScriptPublic;
  PostScriptPublic =
    T OBJECT
    METHODS
      init (filename: Pathname.T; colored := FALSE; portrait := FALSE; ):
            PostScript;
      (* 'portrait' let further calls to setOrientation to be ignored *)
    END;


END LongRealPLPlotStream.
