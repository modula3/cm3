(*******************************************************************************
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.10
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
*******************************************************************************)

INTERFACE QtColumnView;

FROM QtSize IMPORT QSize;
FROM QtAbstractItemModel IMPORT QModelIndex, QAbstractItemModel;
FROM QtWidget IMPORT QWidget;
FROM QtItemSelectionModel IMPORT QItemSelectionModel;
FROM QtAbstractItemView IMPORT ScrollHint;
FROM QtRect IMPORT QRect;


FROM QtAbstractItemView IMPORT QAbstractItemView;

TYPE T = QColumnView;


TYPE
  QColumnView <: QColumnViewPublic;
  QColumnViewPublic =
    QAbstractItemView BRANDED OBJECT
    METHODS
      init_0     (parent: QWidget; ): QColumnView;
      init_1     (): QColumnView;
      scrollTo   (index: QModelIndex; hint: ScrollHint; ); (* virtual *)
      scrollTo1  (index: QModelIndex; ); (* virtual *)
      sizeHint   (): QSize;      (* virtual *)
      visualRect (index: QModelIndex; ): QRect; (* virtual *)
      setModel   (model: QAbstractItemModel; ); (* virtual *)
      setSelectionModel (selectionModel: QItemSelectionModel; ); (* virtual *)
      setRootIndex          (index: QModelIndex; ); (* virtual *)
      selectAll             ();  (* virtual *)
      setResizeGripsVisible (visible: BOOLEAN; );
      resizeGripsVisible    (): BOOLEAN;
      previewWidget         (): QWidget;
      setPreviewWidget      (widget: QWidget; );
      destroyCxx            ();
    END;


END QtColumnView.
