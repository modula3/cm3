INTERFACE QCoreStubs;

TYPE

  QCoreStubs = OBJECT
    cxxObj : ADDRESS;
  METHODS
    init() : QCoreStubs;
    destroyCxx();
  END;

  QRegExp = QCoreStubs;
  QMimeData = QCoreStubs;
  QVariant = QCoreStubs;
  QModelIndexList = QCoreStubs;

END QCoreStubs.