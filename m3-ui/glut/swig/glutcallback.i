
%insert(m3wrapintf) %{
(*
 * Callback functions, see freeglut_callbacks.c
*)
TYPE
  BYTE = [0..255];
  CallBack0T = PROCEDURE();
  CallBack1T = PROCEDURE(p1 : INTEGER);
  CallBack2T = PROCEDURE(p1,p2 : INTEGER);
  CallBack3T = PROCEDURE(p1,p2,p3 : INTEGER);
  CallBack4T = PROCEDURE(p1,p2,p3,p4 : INTEGER);
  CallBack5T = PROCEDURE(p1 : BYTE; p2,p3 : INTEGER);
  CallBack6T = PROCEDURE(p1 : CARDINAL; p2,p3,p4 : INTEGER);

%}

%insert(m3rawintf) %{
TYPE
  BYTE = [0..255];
  CallBack0T = PROCEDURE();
  CallBack1T = PROCEDURE(p1 : INTEGER);
  CallBack2T = PROCEDURE(p1,p2 : INTEGER);
  CallBack3T = PROCEDURE(p1,p2,p3 : INTEGER);
  CallBack4T = PROCEDURE(p1,p2,p3,p4 : INTEGER);
  CallBack5T = PROCEDURE(p1 : BYTE; p2,p3 : INTEGER);
  CallBack6T = PROCEDURE(p1 : CARDINAL; p2,p3,p4 : INTEGER);
%}

//deprecated we dont import raw into safe interface
//%typemap("m3wrapintype:import")  void (*) (void) %{GLUTRaw%}

//these typemaps to remove the VAR for the callback so can pass procedure directly
%typemap(m3wrapinmode)  void (*) (void)  %{%}
%typemap(m3rawinmode)  void (*)  (void) %{%}
%typemap(m3wrapinmode)  void (*) (int) %{%}
%typemap(m3rawinmode)  void (*)  (int) %{%}
%typemap(m3wrapinmode)  void (*) (int, int) %{%}
%typemap(m3rawinmode)  void (*)  (int, int) %{%}
%typemap(m3wrapinmode)  void (*) (int, int, int) %{%}
%typemap(m3rawinmode)  void (*)  (int, int, int) %{%}
%typemap(m3wrapinmode)  void (*) (int, int, int, int) %{%}
%typemap(m3rawinmode)  void (*)  (int, int, int, int) %{%}
%typemap(m3wrapinmode)  void (*) (unsigned char, int, int) %{%}
%typemap(m3rawinmode)  void (*)  (unsigned char, int, int) %{%}
%typemap(m3wrapinmode)  void (*) (unsigned int, int, int, int) %{%}
%typemap(m3rawinmode)  void (*)  (unsigned int, int, int, int) %{%}


%typemap(m3rawintype)   void (*) (void) %{CallBack0T%}
%typemap(m3wrapintype)  void (*) (void) %{CallBack0T%}

%typemap(m3rawintype)   void (*) (int) %{CallBack1T%}
%typemap(m3wrapintype)  void (*) (int) %{CallBack1T%}

%typemap(m3rawintype)   void (*) (int, int) %{CallBack2T%}
%typemap(m3wrapintype)  void (*) (int, int) %{CallBack2T%}

%typemap(m3rawintype)   void (*) (int, int, int) %{CallBack3T%}
%typemap(m3wrapintype)  void (*) (int, int, int) %{CallBack3T%}

%typemap(m3rawintype)   void (*) (int, int, int, int) %{CallBack4T%}
%typemap(m3wrapintype)  void (*) (int, int, int, int) %{CallBack4T%}

%typemap(m3rawintype)   void (*) (unsigned char, int, int) %{CallBack5T%}
%typemap(m3wrapintype)  void (*) (unsigned char, int, int) %{CallBack5T%}

%typemap(m3rawintype)   void (*) (unsigned int, int, int, int) %{CallBack6T%}
%typemap(m3wrapintype)  void (*) (unsigned int, int, int, int) %{CallBack6T%}

//START funcs

FGAPI int     FGAPIENTRY glutCreateMenu( void (* callback)( int menu ) );

/*
 * Global callback functions, see freeglut_callbacks.c
 */
FGAPI void    FGAPIENTRY glutTimerFunc( unsigned int time, void (* callback)( int ), int value );
FGAPI void    FGAPIENTRY glutIdleFunc( void (* callback)( void ) );

/*
 * Window-specific callback functions, see freeglut_callbacks.c
 */
FGAPI void    FGAPIENTRY glutKeyboardFunc( void (* callback)( unsigned char, int, int ) );
FGAPI void    FGAPIENTRY glutSpecialFunc( void (* callback)( int, int, int ) );
FGAPI void    FGAPIENTRY glutReshapeFunc( void (* callback)( int, int ) );
FGAPI void    FGAPIENTRY glutVisibilityFunc( void (* callback)( int ) );
FGAPI void    FGAPIENTRY glutDisplayFunc( void (* callback)( void ) );
FGAPI void    FGAPIENTRY glutMouseFunc( void (* callback)( int, int, int, int ) );
FGAPI void    FGAPIENTRY glutMotionFunc( void (* callback)( int, int ) );
FGAPI void    FGAPIENTRY glutPassiveMotionFunc( void (* callback)( int, int ) );
FGAPI void    FGAPIENTRY glutEntryFunc( void (* callback)( int ) );

FGAPI void    FGAPIENTRY glutKeyboardUpFunc( void (* callback)( unsigned char, int, int ) );
FGAPI void    FGAPIENTRY glutSpecialUpFunc( void (* callback)( int, int, int ) );
FGAPI void    FGAPIENTRY glutJoystickFunc( void (* callback)( unsigned int, int, int, int ), int pollInterval );
FGAPI void    FGAPIENTRY glutMenuStateFunc( void (* callback)( int ) );
FGAPI void    FGAPIENTRY glutMenuStatusFunc( void (* callback)( int, int, int ) );
FGAPI void    FGAPIENTRY glutOverlayDisplayFunc( void (* callback)( void ) );
FGAPI void    FGAPIENTRY glutWindowStatusFunc( void (* callback)( int ) );

FGAPI void    FGAPIENTRY glutSpaceballMotionFunc( void (* callback)( int, int, int ) );
FGAPI void    FGAPIENTRY glutSpaceballRotateFunc( void (* callback)( int, int, int ) );
FGAPI void    FGAPIENTRY glutSpaceballButtonFunc( void (* callback)( int, int ) );
FGAPI void    FGAPIENTRY glutButtonBoxFunc( void (* callback)( int, int ) );
FGAPI void    FGAPIENTRY glutDialsFunc( void (* callback)( int, int ) );
FGAPI void    FGAPIENTRY glutTabletMotionFunc( void (* callback)( int, int ) );
FGAPI void    FGAPIENTRY glutTabletButtonFunc( void (* callback)( int, int, int, int ) );

