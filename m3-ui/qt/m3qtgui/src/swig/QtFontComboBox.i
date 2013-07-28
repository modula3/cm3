%module QtFontComboBox

%include "m3qt.i"
%include "common.i"

%import "QtComboBox.i"

%{
#include <QtGui/qfontcombobox.h>
#define  FontFilters QFontComboBox::FontFilters
%}

%insert(m3rawintf) %{
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{
FROM QtComboBox IMPORT QComboBox;

TYPE
  T = QFontComboBox;
  FontFilters = INTEGER;

%}

%insert(m3wrapimpl) %{
IMPORT WeakRef;
%}

//fix these enums and fontdatabase class later
%ignore writingSystem;
%ignore setWritingSystem;

//Local Enums
EnumMaps(QFontComboBox, FontFilter, ErrMode)

//Local Flags
EnumFlags(QFontComboBox::FontFilters, FontFilters)
EnumFlags(FontFilters, FontFilters)

DoType(QFont,QtFont)

%include <QtGui/qfontcombobox.h>
