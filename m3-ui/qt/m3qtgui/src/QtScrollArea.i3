(*******************************************************************************
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.10
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
*******************************************************************************)

INTERFACE QtScrollArea;

FROM QtSize IMPORT QSize;
FROM QtWidget IMPORT QWidget;
FROM QtNamespace IMPORT AlignmentFlag;


FROM QtAbstractScrollArea IMPORT QAbstractScrollArea;
TYPE T = QScrollArea;


TYPE
  QScrollArea <: QScrollAreaPublic;
  QScrollAreaPublic = QAbstractScrollArea BRANDED OBJECT
                      METHODS
                        init_0          (parent: QWidget; ): QScrollArea;
                        init_1          (): QScrollArea;
                        widget          (): QWidget;
                        setWidget       (widget: QWidget; );
                        takeWidget      (): QWidget;
                        widgetResizable (): BOOLEAN;
                        setWidgetResizable (resizable: BOOLEAN; );
                        sizeHint           (): QSize; (* virtual *)
                        focusNextPrevChild (next: BOOLEAN; ):
                                            BOOLEAN; (* virtual *)
                        alignment      (): AlignmentFlag;
                        setAlignment   (arg1: AlignmentFlag; );
                        ensureVisible  (x, y, xmargin, ymargin: INTEGER; );
                        ensureVisible1 (x, y, xmargin: INTEGER; );
                        ensureVisible2 (x, y: INTEGER; );
                        ensureWidgetVisible (childWidget     : QWidget;
                                             xmargin, ymargin: INTEGER; );
                        ensureWidgetVisible1 (childWidget: QWidget;
                                              xmargin    : INTEGER; );
                        ensureWidgetVisible2 (childWidget: QWidget; );
                        destroyCxx           ();
                      END;


END QtScrollArea.
