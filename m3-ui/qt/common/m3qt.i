// m3qt.i

// common macros and pragmas

%pragma(modula3) library="m3qtcore";
%pragma(modula3) unsafe="true";

# define QT_BEGIN_HEADER
# define QT_BEGIN_NAMESPACE
# define QT_END_HEADER
# define QT_END_NAMESPACE
# define QT_MODULE(Core)

# define QT_MODULE(Gui)
# define Q_CORE_EXPORT
# define Q_GUI_EXPORT
# define Q_PROPERTY(x1 x2 x3 x4 x5 x6)
# define Q_OBJECT

# define Q_SLOTS
# define Q_SIGNALS protected
# define Q_DISABLE_COPY(QLabel)
# define Q_PRIVATE_SLOT(a,b)
# define Q_DECLARE_PRIVATE(a)

# define QT_NO_DATASTREAM
# define QT_NO_DEBUG_STREAM

# define Q_CORE_EXPORT_INLINE

# define Q_REQUIRED_RESULT

# define QT_ASCII_CAST_WARN

# define Q_GADGET
# define Q_ENUMS(a)
# define QDOC_PROPERTY(x1 x2 x3 x4 x5 x6)

//qvector
# define Q_OUTOFLINE_TEMPLATE
# define Q_TYPENAME
# define  Q_DECLARE_SEQUENTIAL_ITERATOR(Vector)
# define Q_DECLARE_MUTABLE_SEQUENTIAL_ITERATOR(x)

//qobject
# define Q_INVOKABLE

//qstring
# define Q_DECLARE_TYPEINFO(QString, Q_MOVABLE_TYPE)
# define Q_DECLARE_SHARED(QString)
# define Q_DECLARE_OPERATORS_FOR_FLAGS(xx)
# define Q_DECLARE_FLAGS(SectionFlags, SectionFlag)
# define QT_ASCII_CAST_WARN_CONSTRUCTOR

//qpixmap
# define  Q_DUMMY_COMPARISON_OPERATOR(x)

//qimage
# define Q_GUI_EXPORT_INLINE

//qwidget
# define QT_MOC_COMPAT

//qmainwindow
# define Q_FLAGS(a)

//qlayout
# define QT_BEGIN_INCLUDE_NAMESPACE
# define QT_END_INCLUDE_NAMESPACE

//qtextbrowser
# define Q_OVERRIDE(a)

