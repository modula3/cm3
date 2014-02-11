INTERFACE QGuiStubs;

IMPORT Ctypes AS C;

TYPE

  Policy = INTEGER;
  ControlTypes = INTEGER;
  ScrollHint = INTEGER;

  MoveOperation = INTEGER;
  MoveMode = INTEGER;
  MoveAnchor = INTEGER;

  FindFlags = INTEGER;
  WrapMode = INTEGER;

  RenderHint = INTEGER;
  RenderHints = INTEGER;
  SceneLayers = INTEGER;
  TabShape = INTEGER;
  TabPosition = INTEGER;

  QByteArray = OBJECT
    cxxObj : ADDRESS;
  METHODS
    init() : QByteArray;
    data() : C.char_star;
    destroyCxx();
  END;

  QString = OBJECT
    cxxObj : ADDRESS;
  METHODS
    init(arg : C.char_star) : QString;
    fromAscii() : C.char_star; (* testing only *)
    toLocal8Bit() : QByteArray;
    destroyCxx();
  END;

  QStubObject = OBJECT
    cxxObj : ADDRESS;
  METHODS
    init() : QStubObject;
    destroyCxx();
  END;

  QTestList = QStubObject;
  QStringList = QStubObject;
  QVariant = QStubObject;
  QSize = QStubObject;
  QStyle = QStubObject;
  QRgb = QStubObject;
  QPixmap = QStubObject;
  QPixmapData = QStubObject;
  QMovie = QStubObject;
  QPicture = QStubObject;
  QPainter = QStubObject;
  QPaintEngine = QStubObject;
  QIODevice = QStubObject;
  QWidget = QStubObject;
  QIcon = QStubObject;
  QInputContext = QStubObject;
  QWindowSurface = QStubObject;
  QTransform = QStubObject;
  QRegion = QStubObject;
  QMatrix = QStubObject;
  QInputMethodEvent = QStubObject;
  QImage = QStubObject;
  QBitmap = QStubObject;
  QPalette = QStubObject;
  QFont = QStubObject;
  QFontInfo = QStubObject;
  QSizePolicy = QStubObject;
  QLocale = QStubObject;
  QFontMetrics = QStubObject;
  QCursor = QStubObject;
  QColor = QStubObject;
  QBrush = QStubObject;
  QGraphicsItem = QStubObject;
  QGraphicsProxyWidget = QStubObject;
  QGraphicsScene = QStubObject;
  QPainterPath = QStubObject;
  QLayoutItem = QStubObject;
  QLayout = QStubObject;
  QMenu = QStubObject;
  QMenuBar = QStubObject;
  QCompleter = QStubObject;
  QValidator = QStubObject;
  QStatusBar = QStubObject;
  QToolBar = QStubObject;
  QOrientation = QStubObject;
  QToolBarArea = QStubObject;
  QPolygon = QStubObject;
  QDockWidget = QStubObject;
  QDate = QStubObject;
  QTime = QStubObject;
  QDateTime = QStubObject;
  QCalendarWidget = QStubObject;
  QMdiArea = QStubObject;
  QTextCharFormat = QStubObject;
  QModelIndex = QStubObject;
  QModelIndexList = QStubObject;
  QStyleOptionViewItem = QStubObject;
  QAction = QStubObject;
  QHelpEvent = QStubObject;
  QActionGroup = QStubObject;
  QScrollBar = QStubObject;
  QWidgetList = QStubObject;
  QAbstractItemModel = QStubObject;
  QItemSelectionModel = QStubObject;
  QUndoStack = QStubObject;
  QUndoGroup = QStubObject;
  QButtonGroup = QStubObject;
  QHeaderView = QStubObject;
  QTextDocument = QStubObject;
  QTextCursor = QStubObject;
  QUrl = QStubObject;
  QPrinter = QStubObject;
  QDir = QStubObject;
  QAbstractItemDelegate = QStubObject;
  QFileIconProvider = QStubObject;
  QAbstractProxyModel = QStubObject;
  QPaintDevice = QStubObject;
  QGradientStops = QStubObject;
  QIconEngine = QStubObject;
  QIconEngineV2 = QStubObject;
  QKeySequence = QStubObject;
  QGraphicsEffect = QStubObject;
  QRegExp = QStubObject;
  QStyleOption = QStubObject;
  QStyleOptionComplex = QStubObject;
  QStyleHintReturn = QStubObject;
  QPen = QStubObject;
  QMouseEvent = QStubObject;
  QChar = QStubObject;

END QGuiStubs.
