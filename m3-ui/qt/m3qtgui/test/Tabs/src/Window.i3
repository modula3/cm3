INTERFACE Window;

FROM QtWidget IMPORT QWidget;
FROM QtDialog IMPORT QDialog;

TYPE
  Window <: WindowPublic;
  WindowPublic = QDialog BRANDED OBJECT
  METHODS
    init(parent : QWidget := NIL) : Window;
  END;

END Window.