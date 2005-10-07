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
             setOrientation        (orient: LONGREAL; );

             exit ();
           END;

  Generic <: GenericPublic;
  GenericPublic = T OBJECT METHODS init (): T; END;

  XWindow <: XWindowPublic;
  XWindowPublic = T OBJECT METHODS init (): T; END;

  PostScript <: PostScriptPublic;
  PostScriptPublic = T OBJECT
                     METHODS
                       init (filename: Pathname.T; colored := FALSE; ): T;
                     END;


END LongRealPLPlotStream.
