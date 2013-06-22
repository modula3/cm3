INTERFACE Window;

FROM QtWidget IMPORT QWidget;
(*
FROM QtMainWindow IMPORT QMainWindow;
*)
FROM QtBoxLayout IMPORT QVBoxLayout;

TYPE
  Window <: WindowPublic;
(*
  WindowPublic = QVBoxLayout BRANDED OBJECT
  *)
  WindowPublic = QWidget BRANDED OBJECT
  METHODS
    init(parent : QWidget := NIL) : Window;
  END;

END Window.