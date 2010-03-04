(* Basic testing of GLUT including creating windows subwindows and callbacks *)

MODULE Main;

IMPORT Thread,Word,GLUT,GL,GLu;

VAR
  argVarr : ARRAY[0..0] OF TEXT;
  win,subWin : INTEGER;
  angle : REAL := 0.0E0;

PROCEDURE SolidTeapot() =
BEGIN

  GLUT.SolidTeapot(0.2D0);

END SolidTeapot;

PROCEDURE Teapot() =
BEGIN
(*
  different objects
  GLUT.WireCube(0.2D0);
  GLUT.SolidCube(0.2D0);
  GLUT.SolidTeapot(0.2D0);
  GLUT.SolidSierpinskiSponge (3, offset, 1.0D0)
*)

  GLUT.WireTeapot(0.2D0);
END Teapot;

PROCEDURE Display() =
BEGIN

  GL.glClearColor (0.0,0.0,0.0,1.0);

  GL.glClear(Word.Or(GL.GL_COLOR_BUFFER_BIT,GL.GL_DEPTH_BUFFER_BIT));
  GL.glLoadIdentity();
  GLu.gluLookAt (0.0D0, 0.0D0, 5.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0);

  (* rotate it *)
  GL.glRotatef(angle, 1.0, 0.0, 0.0); (*rotate on the x axis*)
  GL.glRotatef(angle, 0.0, 1.0, 0.0); (*rotate on the y axis*)
  GL.glRotatef(angle, 0.0, 0.0, 1.0); (*rotate on the z axis*)

  GL.glColor3f(1.0, 0.0, 0.0);
  (* end rotate *)

  SolidTeapot();

  GLUT.SwapBuffers();

  angle := angle + 1.0E0;
END Display;

PROCEDURE SubDisplay() =
BEGIN

  GL.glClearColor (0.0,0.0,0.0,1.0);

  GL.glClear(Word.Or(GL.GL_COLOR_BUFFER_BIT,GL.GL_DEPTH_BUFFER_BIT));
  GL.glLoadIdentity();
  GLu.gluLookAt (0.0D0, 0.0D0, 5.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0);

  (* rotate it *)
  GL.glRotatef(angle, 1.0, 0.0, 0.0); (*rotate on the x axis*)
  GL.glRotatef(angle, 0.0, 1.0, 0.0); (*rotate on the y axis*)
  GL.glRotatef(angle, 0.0, 0.0, 1.0); (*rotate on the z axis*)

  GL.glColor3f(1.0, 0.0, 0.0);
  (* end rotate *)

  Teapot();
  GLUT.SwapBuffers();

  angle := angle + 1.0E0;
END SubDisplay;


PROCEDURE Reshape(w,h : INTEGER) =
BEGIN
  GL.glViewport(0,0,w,h);
  GL.glMatrixMode (GL.GL_PROJECTION);
  GL.glLoadIdentity();
  GLu.gluPerspective(6.0D0, FLOAT(w,LONGREAL) / FLOAT(h,LONGREAL), 1.0D0,100.0D0);
  GL.glMatrixMode (GL.GL_MODELVIEW);
END Reshape;


PROCEDURE SubReshape(w,h : INTEGER) =
BEGIN
  GL.glViewport(0,0,w,h);
  GL.glMatrixMode (GL.GL_PROJECTION);
  GL.glLoadIdentity();
  GLu.gluPerspective(6.0D0, FLOAT(w,LONGREAL) / FLOAT(h,LONGREAL), 1.0D0,100.0D0);
  GL.glMatrixMode (GL.GL_MODELVIEW);
END SubReshape;

PROCEDURE Idle() =
BEGIN
  GLUT.SetWindow(win);
  GLUT.PostRedisplay();
  GLUT.SetWindow(subWin);
  GLUT.PostRedisplay();

  Thread.Pause(1.0D0 / 25.0D0);
END Idle;


BEGIN

  GLUT.Init(0,argVarr);
  GLUT.InitDisplayMode(Word.Or(Word.Or(GLUT.GLUT_RGB,GLUT.GLUT_DOUBLE),GLUT.GLUT_DEPTH));

  GLUT.InitWindowSize(800,800);
  GLUT.InitWindowPosition(100,100);

  win := GLUT.CreateWindow("Basic Window");

(* enable depth lighting *)
  GL.glClearDepth(1.0D0);
  GL.glEnable (GL.GL_DEPTH_TEST);
  GL.glEnable (GL.GL_LIGHTING);
  GL.glEnable (GL.GL_LIGHT0);

(* smoothing of lines *)
  GL.glEnable (GL.GL_LINE_SMOOTH);
  GL.glEnable (GL.GL_BLEND);
  GL.glBlendFunc (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
  GL.glHint (GL.GL_LINE_SMOOTH_HINT, GL.GL_DONT_CARE);

(* callbacks *)
  GLUT.DisplayFunc(Display);
  GLUT.ReshapeFunc(Reshape);
  GLUT.IdleFunc(Idle);

(* create a subwindow with own callbacks*)
  subWin := GLUT.CreateSubWindow( win, 10, 10, 400, 400);
  GLUT.DisplayFunc(SubDisplay);
  GLUT.ReshapeFunc(SubReshape);

  GL.glClearDepth(1.0D0);
(* enbale depth testing and lighting *)
  GL.glEnable (GL.GL_DEPTH_TEST);
  GL.glEnable (GL.GL_LIGHTING);
  GL.glEnable (GL.GL_LIGHT0);

  GLUT.MainLoop();

END Main.
