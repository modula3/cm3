INTERFACE Window;

FROM QtWidget IMPORT QWidget;
FROM QtMainWindow IMPORT QMainWindow;

TYPE
  Window <: WindowPublic;
  WindowPublic = QMainWindow BRANDED OBJECT
  METHODS
    init(parent : QWidget := NIL) : Window;
  END;

END Window.