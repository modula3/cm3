%module GLUT

%pragma(modula3) library="GLUT";
%pragma(modula3) unsafe="true";


#define FGAPI
#define FGAPIENTRY


// not necessary to import Ctypes as done be default IMPORT Ctypes AS C;

%insert(m3makefile) %{
import ("opengl")
import_lib("glut","/usr/lib")
%}

%insert(m3wrapimpl) %{
VAR
  m3argc : C.int;
  m3argv : UNTRACED REF ARRAY OF C.char_star;
%}

%insert(m3wrapintf) %{

(*
/*
 * The freeglut and GLUT API versions
 */
#define  FREEGLUT             1
#define  GLUT_API_VERSION     4
#define  FREEGLUT_VERSION_2_0 1
#define  GLUT_XLIB_IMPLEMENTATION 13
*)

CONST

(*
 * GLUT API macro definitions -- the special key codes:
 *)

  GLUT_KEY_F1                        = 16_0001;
  GLUT_KEY_F2                        = 16_0002;
  GLUT_KEY_F3                        = 16_0003;
  GLUT_KEY_F4                        = 16_0004;
  GLUT_KEY_F5                        = 16_0005;
  GLUT_KEY_F6                        = 16_0006;
  GLUT_KEY_F7                        = 16_0007;
  GLUT_KEY_F8                        = 16_0008;
  GLUT_KEY_F9                        = 16_0009;
  GLUT_KEY_F10                       = 16_000A;
  GLUT_KEY_F11                       = 16_000B;
  GLUT_KEY_F12                       = 16_000C;
  GLUT_KEY_LEFT                      = 16_0064;
  GLUT_KEY_UP                        = 16_0065;
  GLUT_KEY_RIGHT                     = 16_0066;
  GLUT_KEY_DOWN                      = 16_0067;
  GLUT_KEY_PAGE_UP                   = 16_0068;
  GLUT_KEY_PAGE_DOWN                 = 16_0069;
  GLUT_KEY_HOME                      = 16_006A;
  GLUT_KEY_END                       = 16_006B;
  GLUT_KEY_INSERT                    = 16_006C;

(*
 * GLUT API macro definitions -- mouse state definitions
 *)

  GLUT_LEFT_BUTTON                   = 16_0000;
  GLUT_MIDDLE_BUTTON                 = 16_0001;
  GLUT_RIGHT_BUTTON                  = 16_0002;
  GLUT_DOWN                          = 16_0000;
  GLUT_UP                            = 16_0001;
  GLUT_LEFT                          = 16_0000;
  GLUT_ENTERED                       = 16_0001;

(*
 * GLUT API macro definitions -- the display mode definitions
 *)

  GLUT_RGB                           = 16_0000;
  GLUT_RGBA                          = 16_0000;
  GLUT_INDEX                         = 16_0001;
  GLUT_SINGLE                        = 16_0000;
  GLUT_DOUBLE                        = 16_0002;
  GLUT_ACCUM                         = 16_0004;
  GLUT_ALPHA                         = 16_0008;
  GLUT_DEPTH                         = 16_0010;
  GLUT_STENCIL                       = 16_0020;
  GLUT_MULTISAMPLE                   = 16_0080;
  GLUT_STEREO                        = 16_0100;
  GLUT_LUMINANCE                     = 16_0200;

(*
 * GLUT API macro definitions -- windows and menu related definitions
 *)

  GLUT_MENU_NOT_IN_USE               = 16_0000;
  GLUT_MENU_IN_USE                   = 16_0001;
  GLUT_NOT_VISIBLE                   = 16_0000;
  GLUT_VISIBLE                       = 16_0001;
  GLUT_HIDDEN                        = 16_0000;
  GLUT_FULLY_RETAINED                = 16_0001;
  GLUT_PARTIALLY_RETAINED            = 16_0002;
  GLUT_FULLY_COVERED                 = 16_0003;

(*
 * GLUT API macro definitions -- the glutGet parameters
 *)

  GLUT_WINDOW_X                      = 16_0064;
  GLUT_WINDOW_Y                      = 16_0065;
  GLUT_WINDOW_WIDTH                  = 16_0066;
  GLUT_WINDOW_HEIGHT                 = 16_0067;
  GLUT_WINDOW_BUFFER_SIZE            = 16_0068;
  GLUT_WINDOW_STENCIL_SIZE           = 16_0069;
  GLUT_WINDOW_DEPTH_SIZE             = 16_006A;
  GLUT_WINDOW_RED_SIZE               = 16_006B;
  GLUT_WINDOW_GREEN_SIZE             = 16_006C;
  GLUT_WINDOW_BLUE_SIZE              = 16_006D;
  GLUT_WINDOW_ALPHA_SIZE             = 16_006E;
  GLUT_WINDOW_ACCUM_RED_SIZE         = 16_006F;
  GLUT_WINDOW_ACCUM_GREEN_SIZE       = 16_0070;
  GLUT_WINDOW_ACCUM_BLUE_SIZE        = 16_0071;
  GLUT_WINDOW_ACCUM_ALPHA_SIZE       = 16_0072;
  GLUT_WINDOW_DOUBLEBUFFER           = 16_0073;
  GLUT_WINDOW_RGBA                   = 16_0074;
  GLUT_WINDOW_PARENT                 = 16_0075;
  GLUT_WINDOW_NUM_CHILDREN           = 16_0076;
  GLUT_WINDOW_COLORMAP_SIZE          = 16_0077;
  GLUT_WINDOW_NUM_SAMPLES            = 16_0078;
  GLUT_WINDOW_STEREO                 = 16_0079;
  GLUT_WINDOW_CURSOR                 = 16_007A;

  GLUT_SCREEN_WIDTH                  = 16_00C8;
  GLUT_SCREEN_HEIGHT                 = 16_00C9;
  GLUT_SCREEN_WIDTH_MM               = 16_00CA;
  GLUT_SCREEN_HEIGHT_MM              = 16_00CB;
  GLUT_MENU_NUM_ITEMS                = 16_012C;
  GLUT_DISPLAY_MODE_POSSIBLE         = 16_0190;
  GLUT_INIT_WINDOW_X                 = 16_01F4;
  GLUT_INIT_WINDOW_Y                 = 16_01F5;
  GLUT_INIT_WINDOW_WIDTH             = 16_01F6;
  GLUT_INIT_WINDOW_HEIGHT            = 16_01F7;
  GLUT_INIT_DISPLAY_MODE             = 16_01F8;
  GLUT_ELAPSED_TIME                  = 16_02BC;
  GLUT_WINDOW_FORMAT_ID              = 16_007B;
  GLUT_INIT_STATE                    = 16_007C;

(*
 * GLUT API macro definitions -- the glutDeviceGet parameters
 *)

  GLUT_HAS_KEYBOARD                  = 16_0258;
  GLUT_HAS_MOUSE                     = 16_0259;
  GLUT_HAS_SPACEBALL                 = 16_025A;
  GLUT_HAS_DIAL_AND_BUTTON_BOX       = 16_025B;
  GLUT_HAS_TABLET                    = 16_025C;
  GLUT_NUM_MOUSE_BUTTONS             = 16_025D;
  GLUT_NUM_SPACEBALL_BUTTONS         = 16_025E;
  GLUT_NUM_BUTTON_BOX_BUTTONS        = 16_025F;
  GLUT_NUM_DIALS                     = 16_0260;
  GLUT_NUM_TABLET_BUTTONS            = 16_0261;
  GLUT_DEVICE_IGNORE_KEY_REPEAT      = 16_0262;
  GLUT_DEVICE_KEY_REPEAT             = 16_0263;
  GLUT_HAS_JOYSTICK                  = 16_0264;
  GLUT_OWNS_JOYSTICK                 = 16_0265;
  GLUT_JOYSTICK_BUTTONS              = 16_0266;
  GLUT_JOYSTICK_AXES                 = 16_0267;
  GLUT_JOYSTICK_POLL_RATE            = 16_0268;

(*
 * GLUT API macro definitions -- the glutLayerGet parameters
 *)

  GLUT_OVERLAY_POSSIBLE              = 16_0320;
  GLUT_LAYER_IN_USE                  = 16_0321;
  GLUT_HAS_OVERLAY                   = 16_0322;
  GLUT_TRANSPARENT_INDEX             = 16_0323;
  GLUT_NORMAL_DAMAGED                = 16_0324;
  GLUT_OVERLAY_DAMAGED               = 16_0325;

(*
 * GLUT API macro definitions -- the glutVideoResizeGet parameters
 *)

  GLUT_VIDEO_RESIZE_POSSIBLE         = 16_0384;
  GLUT_VIDEO_RESIZE_IN_USE           = 16_0385;
  GLUT_VIDEO_RESIZE_X_DELTA          = 16_0386;
  GLUT_VIDEO_RESIZE_Y_DELTA          = 16_0387;
  GLUT_VIDEO_RESIZE_WIDTH_DELTA      = 16_0388;
  GLUT_VIDEO_RESIZE_HEIGHT_DELTA     = 16_0389;
  GLUT_VIDEO_RESIZE_X                = 16_038A;
  GLUT_VIDEO_RESIZE_Y                = 16_038B;
  GLUT_VIDEO_RESIZE_WIDTH            = 16_038C;
  GLUT_VIDEO_RESIZE_HEIGHT           = 16_038D;

(*
 * GLUT API macro definitions -- the glutUseLayer parameters
 *)

  GLUT_NORMAL                        = 16_0000;
  GLUT_OVERLAY                       = 16_0001;

(*
 * GLUT API macro definitions -- the glutGetModifiers parameters
 *)

  GLUT_ACTIVE_SHIFT                  = 16_0001;
  GLUT_ACTIVE_CTRL                   = 16_0002;
  GLUT_ACTIVE_ALT                    = 16_0004;

(*
 * GLUT API macro definitions -- the glutSetCursor parameters
 *)

  GLUT_CURSOR_RIGHT_ARROW            = 16_0000;
  GLUT_CURSOR_LEFT_ARROW             = 16_0001;
  GLUT_CURSOR_INFO                   = 16_0002;
  GLUT_CURSOR_DESTROY                = 16_0003;
  GLUT_CURSOR_HELP                   = 16_0004;
  GLUT_CURSOR_CYCLE                  = 16_0005;
  GLUT_CURSOR_SPRAY                  = 16_0006;
  GLUT_CURSOR_WAIT                   = 16_0007;
  GLUT_CURSOR_TEXT                   = 16_0008;
  GLUT_CURSOR_CROSSHAIR              = 16_0009;
  GLUT_CURSOR_UP_DOWN                = 16_000A;
  GLUT_CURSOR_LEFT_RIGHT             = 16_000B;
  GLUT_CURSOR_TOP_SIDE               = 16_000C;
  GLUT_CURSOR_BOTTOM_SIDE            = 16_000D;
  GLUT_CURSOR_LEFT_SIDE              = 16_000E;
  GLUT_CURSOR_RIGHT_SIDE             = 16_000F;
  GLUT_CURSOR_TOP_LEFT_CORNER        = 16_0010;
  GLUT_CURSOR_TOP_RIGHT_CORNER       = 16_0011;
  GLUT_CURSOR_BOTTOM_RIGHT_CORNER    = 16_0012;
  GLUT_CURSOR_BOTTOM_LEFT_CORNER     = 16_0013;
  GLUT_CURSOR_INHERIT                = 16_0064;
  GLUT_CURSOR_NONE                   = 16_0065;
  GLUT_CURSOR_FULL_CROSSHAIR         = 16_0066;

(*
 * GLUT API macro definitions -- RGB color component specification definitions
 *)

  GLUT_RED                           = 16_0000;
  GLUT_GREEN                         = 16_0001;
  GLUT_BLUE                          = 16_0002;

(*
 * GLUT API macro definitions -- additional keyboard and joystick definitions
 *)

  GLUT_KEY_REPEAT_OFF                = 16_0000;
  GLUT_KEY_REPEAT_ON                 = 16_0001;
  GLUT_KEY_REPEAT_DEFAULT            = 16_0002;

  GLUT_JOYSTICK_BUTTON_A             = 16_0001;
  GLUT_JOYSTICK_BUTTON_B             = 16_0002;
  GLUT_JOYSTICK_BUTTON_C             = 16_0004;
  GLUT_JOYSTICK_BUTTON_D             = 16_0008;

(*
 * GLUT API macro definitions -- game mode definitions
 *)

  GLUT_GAME_MODE_ACTIVE              = 16_0000;
  GLUT_GAME_MODE_POSSIBLE            = 16_0001;
  GLUT_GAME_MODE_WIDTH               = 16_0002;
  GLUT_GAME_MODE_HEIGHT              = 16_0003;
  GLUT_GAME_MODE_PIXEL_DEPTH         = 16_0004;
  GLUT_GAME_MODE_REFRESH_RATE        = 16_0005;
  GLUT_GAME_MODE_DISPLAY_CHANGED     = 16_0006;

%}

//remove dependancy on GL and typedef the GLenum to its base GL type
typedef unsigned int     GLenum;

//%typemap("m3rawintype")   GLenum %{GL.GLenum%}
//%typemap("m3wrapintype")  GLenum %{GL.GLenum%}

//map the GLint,GLfloat,GLdouble types to M3 types

%typemap("m3rawintype")   GLint %{INTEGER%}
%typemap("m3wrapintype")  GLint %{INTEGER%}

%typemap("m3rawintype")   GLdouble %{LONGREAL%}
%typemap("m3wrapintype")  GLdouble %{LONGREAL%}

%typemap("m3rawintype")   GLfloat %{REAL%}
%typemap("m3wrapintype")  GLfloat %{REAL%}
%typemap("m3wraprettype") GLfloat %{REAL%}

//the GetColor raw return type
%typemap("m3rawrettype")  GLfloat %{REAL%}


//strip the glut prefix from all names
%rename("%(strip:[glut])s") "";

//HENCE we dont need all these renames

//RENAMES
//generated with swig -generaterename m3rename -modula3 glut.i
//awk '{print "%rename("$2")" $2 }' m3rename.i > renames.out
//then some replacements

/*
%rename("Init")  glutInit;
%rename("InitWindowPosition")  glutInitWindowPosition;
%rename("InitWindowSize")  glutInitWindowSize;
... lots removed
*/

/*
 * Initialization functions, see fglut_init.c
 */
//see glutinit.i for glutInit

%include glutinit.i

FGAPI void    FGAPIENTRY glutInitWindowPosition( int x, int y );
FGAPI void    FGAPIENTRY glutInitWindowSize( int width, int height );
FGAPI void    FGAPIENTRY glutInitDisplayMode( unsigned int displayMode );
FGAPI void    FGAPIENTRY glutInitDisplayString( const char* displayMode );

/*
 * Process loop function, see freeglut_main.c
 */
FGAPI void    FGAPIENTRY glutMainLoop( void );

/*
 * Window management functions, see freeglut_window.c
 */
FGAPI int     FGAPIENTRY glutCreateWindow( const char* title );
FGAPI int     FGAPIENTRY glutCreateSubWindow( int window, int x, int y, int width, int height );
FGAPI void    FGAPIENTRY glutDestroyWindow( int window );
FGAPI void    FGAPIENTRY glutSetWindow( int window );
FGAPI int     FGAPIENTRY glutGetWindow( void );
FGAPI void    FGAPIENTRY glutSetWindowTitle( const char* title );
FGAPI void    FGAPIENTRY glutSetIconTitle( const char* title );
FGAPI void    FGAPIENTRY glutReshapeWindow( int width, int height );
FGAPI void    FGAPIENTRY glutPositionWindow( int x, int y );
FGAPI void    FGAPIENTRY glutShowWindow( void );
FGAPI void    FGAPIENTRY glutHideWindow( void );
FGAPI void    FGAPIENTRY glutIconifyWindow( void );
FGAPI void    FGAPIENTRY glutPushWindow( void );
FGAPI void    FGAPIENTRY glutPopWindow( void );
FGAPI void    FGAPIENTRY glutFullScreen( void );

/*
 * Display-connected functions, see freeglut_display.c
 */
FGAPI void    FGAPIENTRY glutPostWindowRedisplay( int window );
FGAPI void    FGAPIENTRY glutPostRedisplay( void );
FGAPI void    FGAPIENTRY glutSwapBuffers( void );

/*
 * Mouse cursor functions, see freeglut_cursor.c
 */
FGAPI void    FGAPIENTRY glutWarpPointer( int x, int y );
FGAPI void    FGAPIENTRY glutSetCursor( int cursor );

/*
 * Overlay stuff, see freeglut_overlay.c
 */
FGAPI void    FGAPIENTRY glutEstablishOverlay( void );
FGAPI void    FGAPIENTRY glutRemoveOverlay( void );
FGAPI void    FGAPIENTRY glutUseLayer( GLenum layer );
FGAPI void    FGAPIENTRY glutPostOverlayRedisplay( void );
FGAPI void    FGAPIENTRY glutPostWindowOverlayRedisplay( int window );
FGAPI void    FGAPIENTRY glutShowOverlay( void );
FGAPI void    FGAPIENTRY glutHideOverlay( void );

/*
 * Menu stuff, see freeglut_menu.c
 */
//see glutcallback.i for glutCreateMenu
//FGAPI int     FGAPIENTRY glutCreateMenu( void (* callback)( int menu ) );
FGAPI void    FGAPIENTRY glutDestroyMenu( int menu );
FGAPI int     FGAPIENTRY glutGetMenu( void );
FGAPI void    FGAPIENTRY glutSetMenu( int menu );
FGAPI void    FGAPIENTRY glutAddMenuEntry( const char* label, int value );
FGAPI void    FGAPIENTRY glutAddSubMenu( const char* label, int subMenu );
FGAPI void    FGAPIENTRY glutChangeToMenuEntry( int item, const char* label, int value );
FGAPI void    FGAPIENTRY glutChangeToSubMenu( int item, const char* label, int value );
FGAPI void    FGAPIENTRY glutRemoveMenuItem( int item );
FGAPI void    FGAPIENTRY glutAttachMenu( int button );
FGAPI void    FGAPIENTRY glutDetachMenu( int button );



/*
 * Global callback functions, see freeglut_callbacks.c
 */

%include glutcallback.i

/*
 * Window-specific callback functions, see freeglut_callbacks.c
 */
//see glutcallback.i


/*
 * State setting and retrieval functions, see freeglut_state.c
 */
FGAPI int     FGAPIENTRY glutGet( GLenum query );
FGAPI int     FGAPIENTRY glutDeviceGet( GLenum query );
FGAPI int     FGAPIENTRY glutGetModifiers( void );
FGAPI int     FGAPIENTRY glutLayerGet( GLenum query );


/*
 * Geometry functions, see freeglut_geometry.c
 */
FGAPI void    FGAPIENTRY glutWireCube( GLdouble size );
FGAPI void    FGAPIENTRY glutSolidCube( GLdouble size );
FGAPI void    FGAPIENTRY glutWireSphere( GLdouble radius, GLint slices, GLint stacks );
FGAPI void    FGAPIENTRY glutSolidSphere( GLdouble radius, GLint slices, GLint stacks );
FGAPI void    FGAPIENTRY glutWireCone( GLdouble base, GLdouble height, GLint slices, GLint stacks );
FGAPI void    FGAPIENTRY glutSolidCone( GLdouble base, GLdouble height, GLint slices, GLint stacks );

FGAPI void    FGAPIENTRY glutWireTorus( GLdouble innerRadius, GLdouble outerRadius, GLint sides, GLint rings );
FGAPI void    FGAPIENTRY glutSolidTorus( GLdouble innerRadius, GLdouble outerRadius, GLint sides, GLint rings );
FGAPI void    FGAPIENTRY glutWireDodecahedron( void );
FGAPI void    FGAPIENTRY glutSolidDodecahedron( void );
FGAPI void    FGAPIENTRY glutWireOctahedron( void );
FGAPI void    FGAPIENTRY glutSolidOctahedron( void );
FGAPI void    FGAPIENTRY glutWireTetrahedron( void );
FGAPI void    FGAPIENTRY glutSolidTetrahedron( void );
FGAPI void    FGAPIENTRY glutWireIcosahedron( void );
FGAPI void    FGAPIENTRY glutSolidIcosahedron( void );

/*
 * Teapot rendering functions, found in freeglut_teapot.c
 */
FGAPI void    FGAPIENTRY glutWireTeapot( GLdouble size );
FGAPI void    FGAPIENTRY glutSolidTeapot( GLdouble size );

/*
 * Game mode functions, see freeglut_gamemode.c
 */
FGAPI void    FGAPIENTRY glutGameModeString( const char* string );
FGAPI int     FGAPIENTRY glutEnterGameMode( void );
FGAPI void    FGAPIENTRY glutLeaveGameMode( void );
FGAPI int     FGAPIENTRY glutGameModeGet( GLenum query );

/*
 * Video resize functions, see freeglut_videoresize.c
 */
FGAPI int     FGAPIENTRY glutVideoResizeGet( GLenum query );
FGAPI void    FGAPIENTRY glutSetupVideoResizing( void );
FGAPI void    FGAPIENTRY glutStopVideoResizing( void );
FGAPI void    FGAPIENTRY glutVideoResize( int x, int y, int width, int height );
FGAPI void    FGAPIENTRY glutVideoPan( int x, int y, int width, int height );

/*
 * Colormap functions, see freeglut_misc.c
 */
FGAPI void    FGAPIENTRY glutSetColor( int color, GLfloat red, GLfloat green, GLfloat blue );
FGAPI GLfloat FGAPIENTRY glutGetColor( int color, int component );
FGAPI void    FGAPIENTRY glutCopyColormap( int window );

/*
 * Misc keyboard and joystick functions, see freeglut_misc.c
 */
FGAPI void    FGAPIENTRY glutIgnoreKeyRepeat( int ignore );
FGAPI void    FGAPIENTRY glutSetKeyRepeat( int repeatMode );
FGAPI void    FGAPIENTRY glutForceJoystickFunc( void );

/*
 * Misc functions, see freeglut_misc.c
 */
FGAPI int     FGAPIENTRY glutExtensionSupported( const char* extension );
FGAPI void    FGAPIENTRY glutReportErrors( void );

//The extension library

%include glutext.i

/*
 * Font stuff, see freeglut_font.c
 *
*/

%include glutfont.i