
INTERFACE SQLtypes;

FROM Ctypes IMPORT unsigned_char, signed_char, long,
  unsigned_long, short, unsigned_short, void_star, double, float;
FROM Cstdint IMPORT uint32_t, int32_t;
FROM WinDef IMPORT HWND;

TYPE
  UCHAR    = unsigned_char;
  SCHAR    = signed_char;
  SDWORD   = long;
  SWORD    = short;
  UDWORD   = unsigned_long;
  UWORD    = unsigned_short;

  SLONG    = long;
  SSHORT   = short;
  ULONG    = unsigned_long;
  USHORT   = unsigned_short;

  LDOUBLE  = double;
  SDOUBLE  = double;
  SFLOAT   = float;

  PTR      = void_star;
  HENV     = void_star;
  HDBC     = void_star;
  HSTMT    = void_star;

  RETCODE  = short;

  SQLCHAR      = UCHAR;     SQLCHAR_star      = UNTRACED REF SQLCHAR;
  SQLSCHAR     = SCHAR;     SQLSCHAR_star     = UNTRACED REF SQLSCHAR;
  SQLINTEGER   = int32_t;   SQLINTEGER_star   = UNTRACED REF SQLINTEGER;
  SQLSMALLINT  = SWORD;     SQLSMALLINT_star  = UNTRACED REF SQLSMALLINT;
  SQLUINTEGER  = uint32_t;  SQLUINTEGER_star  = UNTRACED REF SQLUINTEGER;
  SQLUSMALLINT = UWORD;     SQLUSMALLINT_star = UNTRACED REF SQLUSMALLINT;
  SQLPOINTER   = void_star; SQLPOINTER_star   = UNTRACED REF SQLPOINTER;

  SQLHENV   = HENV;         SQLHENV_star  = UNTRACED REF SQLHENV;
  SQLHDBC   = HDBC;         SQLHDBC_star  = UNTRACED REF SQLHDBC;
  SQLHSTMT  = HSTMT;        SQLHSTMT_star = UNTRACED REF SQLHSTMT;
  SQLRETURN = SQLSMALLINT;
  SQLHWND   = HWND;

  DATE_STRUCT = RECORD
    year  : SQLSMALLINT;
    month : SQLUSMALLINT;
    day   : SQLUSMALLINT;
  END;

  TIME_STRUCT = RECORD
    hour   : SQLUSMALLINT;
    minute : SQLUSMALLINT;
    second : SQLUSMALLINT;
  END;

  TIMESTAMP_STRUCT = RECORD
    year     : SQLSMALLINT;
    month    : SQLUSMALLINT;
    day      : SQLUSMALLINT;
    hour     : SQLUSMALLINT;
    minute   : SQLUSMALLINT;
    second   : SQLUSMALLINT;
    fraction : SQLUINTEGER;
  END;

  BOOKMARK = unsigned_long;

END SQLtypes.
