INTERFACE LongRealPLPlotLineStyle;

IMPORT LongRealPLPlot AS PL;

TYPE
  T = BRANDED OBJECT
      METHODS
        apply ();                (* call the right PLPlot command for
                                    setting the line style *)
      END;

  Default <: DefaultPublic;
  DefaultPublic = T BRANDED OBJECT style: PL.LineStyle;  END;

  Custom <: CustomPublic;
  CustomPublic = T BRANDED OBJECT mark, space: REF ARRAY OF CARDINAL;  END;

VAR Continuous: T;

END LongRealPLPlotLineStyle.
