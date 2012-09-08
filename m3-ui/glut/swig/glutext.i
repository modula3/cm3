// freeglut_ext.h

/*
  This set of defs depends on glut.i, glutfont.i and glutcallback.i

*/

%typemap(m3rawintype)     GLdouble offset[3] %{ARRAY [0..2] OF LONGREAL%}
%typemap(m3wrapintype)    GLdouble offset[3] %{ARRAY [0..2] OF LONGREAL%}

%typemap("m3rawrettype")  GLUTproc %{REF CallBack0T%}
%typemap("m3wraprettype") GLUTproc %{REF CallBack0T%}

//For the WindowData functions
%typemap("m3rawrettype")  void * %{REFANY%}
%typemap("m3wraprettype") void * %{REFANY%}
%typemap("m3rawintype")   void * %{REFANY%}
%typemap("m3wrapintype")  void * %{REFANY%}


%insert(m3wrapintf) %{

(*
  freeglut_ext.h

 GLUT API Extension macro definitions -- behaviour when the user clicks on an "x" to close a window

*)

CONST

 GLUT_ACTION_EXIT                    =  0;
 GLUT_ACTION_GLUTMAINLOOP_RETURNS    =  1;
 GLUT_ACTION_CONTINUE_EXECUTION      =  2;

(*
 * Create a new rendering context when the user opens a new window?
 *)

 GLUT_CREATE_NEW_CONTEXT             =  0;
 GLUT_USE_CURRENT_CONTEXT            =  1;

(*
 * Direct/Indirect rendering context options (has meaning only in Unix/X11)
 *)
 GLUT_FORCE_INDIRECT_CONTEXT         =  0;
 GLUT_ALLOW_DIRECT_CONTEXT           =  1;
 GLUT_TRY_DIRECT_CONTEXT             =  2;
 GLUT_FORCE_DIRECT_CONTEXT           =  3;

(*
 * GLUT API Extension macro definitions -- the glutGet parameters
 *)
  GLUT_ACTION_ON_WINDOW_CLOSE        = 16_01F9;

  GLUT_WINDOW_BORDER_WIDTH           = 16_01FA;
  GLUT_WINDOW_HEADER_HEIGHT          = 16_01FB;

  GLUT_VERSION                       = 16_01FC;

  GLUT_RENDERING_CONTEXT             = 16_01FD;
  GLUT_DIRECT_RENDERING              = 16_01FE;

(*
 * New tokens for glutInitDisplayMode.
 * Only one GLUT_AUXn bit may be used at a time.
 * Value = 0x0400 is defined in OpenGLUT.
 *)
  GLUT_AUX1                          = 16_1000;
  GLUT_AUX2                          = 16_2000;
  GLUT_AUX3                          = 16_4000;
  GLUT_AUX4                          = 16_8000;

%}


/*
 * Process loop function, see freeglut_main.c
 */
FGAPI void    FGAPIENTRY glutMainLoopEvent( void );
FGAPI void    FGAPIENTRY glutLeaveMainLoop( void );

/*
 * Window-specific callback functions, see freeglut_callbacks.c
 */
FGAPI void    FGAPIENTRY glutMouseWheelFunc( void (* callback)( int, int, int, int ) );
FGAPI void    FGAPIENTRY glutCloseFunc( void (* callback)( void ) );
FGAPI void    FGAPIENTRY glutWMCloseFunc( void (* callback)( void ) );
/* A. Donev: Also a destruction callback for menus */
FGAPI void    FGAPIENTRY glutMenuDestroyFunc( void (* callback)( void ) );

/*
 * State setting and retrieval functions, see freeglut_state.c
 */
FGAPI void    FGAPIENTRY glutSetOption ( GLenum option_flag, int value ) ;
/* A.Donev: User-data manipulation */

FGAPI void*   FGAPIENTRY glutGetWindowData( void );
FGAPI void    FGAPIENTRY glutSetWindowData(void* data);
FGAPI void*   FGAPIENTRY glutGetMenuData( void );
FGAPI void    FGAPIENTRY glutSetMenuData(void* data);


/*
 * Geometry functions, see freeglut_geometry.c
 */
FGAPI void    FGAPIENTRY glutWireRhombicDodecahedron( void );
FGAPI void    FGAPIENTRY glutSolidRhombicDodecahedron( void );
FGAPI void    FGAPIENTRY glutWireSierpinskiSponge ( int num_levels, GLdouble offset[3], GLdouble scale ) ;
FGAPI void    FGAPIENTRY glutSolidSierpinskiSponge ( int num_levels, GLdouble offset[3], GLdouble scale ) ;
FGAPI void    FGAPIENTRY glutWireCylinder( GLdouble radius, GLdouble height, GLint slices, GLint stacks);
FGAPI void    FGAPIENTRY glutSolidCylinder( GLdouble radius, GLdouble height, GLint slices, GLint stacks);

/*
 * Extension functions, see freeglut_ext.c
 */
//typedef void (*GLUTproc)();
FGAPI GLUTproc FGAPIENTRY glutGetProcAddress( const char *procName );
