(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed Oct  4 18:10:09 PDT 1995 by najork                   *)
(*       Created on Fri Feb  3 23:25:51 PST 1995 by najork                   *)

(* Modula-3 version of "glu.h" for Win32. *)

INTERFACE GLu;

IMPORT Ctypes, GL;

TYPE
  GLUquadricObjStar      = ADDRESS;
  GLUtriangulatorObjStar = ADDRESS;
  GLUnurbsObjStar        = ADDRESS;

  GLUquadricErrorProc = <*CALLBACK*> PROCEDURE (err: GL.GLenum);
  GLUtessBeginProc    = <*CALLBACK*> PROCEDURE (a1: GL.GLenum);
  GLUtessEdgeProc     = <*CALLBACK*> PROCEDURE (a1: GL.GLboolean);
  GLUtessVertexProc   = <*CALLBACK*> PROCEDURE (a1: GL.GLvoidStar);
  GLUtessEndProc      = <*CALLBACK*> PROCEDURE ();
  GLUtessErrorProc    = <*CALLBACK*> PROCEDURE (a1: GL.GLenum);
  GLUtessAnyProc      = <*CALLBACK*> PROCEDURE ();
  GLUnurbsErrorProc   = <*CALLBACK*> PROCEDURE (err: GL.GLenum);


(*
 * Return the error string associated with a particular error code.
 * This will return 0 for an invalid error code.
 *)

<*EXTERNAL gluErrorString:WINAPI*>
PROCEDURE gluErrorString (errorCode: GL.GLenum): GL.GLubyteStar;

<*EXTERNAL gluOrtho2D:WINAPI*>
PROCEDURE gluOrtho2D (left, right, bottom, top: GL.GLdouble);

<*EXTERNAL gluPerspective:WINAPI*>
PROCEDURE gluPerspective (fovy, aspect, zNear, zFar: GL.GLdouble);

<*EXTERNAL gluPickMatrix:WINAPI*>
PROCEDURE gluPickMatrix (x, y, width, height: GL.GLdouble; 
                         viewport: UNTRACED REF ARRAY [1..4] OF GL.GLint);

<*EXTERNAL gluLookAt:WINAPI*>
PROCEDURE gluLookAt (eyex,    eyey,    eyez   : GL.GLdouble; 
                     centerx, centery, centerz: GL.GLdouble; 
                     upx,     upy,     upz    : GL.GLdouble);

<*EXTERNAL gluProject:WINAPI*>
PROCEDURE gluProject (
    objx, objy, objz: GL.GLdouble;
    modelMatrix     : UNTRACED REF ARRAY [1..16] OF GL.GLdouble;
    projMatrix      : UNTRACED REF ARRAY [1..16] OF GL.GLdouble;
    viewport        : UNTRACED REF ARRAY [1..4] OF GL.GLint;
    winx, winy, winz: GL.GLdoubleStar): Ctypes.int;

<*EXTERNAL gluUnProject:WINAPI*>
PROCEDURE gluUnProject (
    winx, winy, winz: GL.GLdouble;
    modelMatrix     : UNTRACED REF ARRAY [1..16] OF GL.GLdouble;
    projMatrix      : UNTRACED REF ARRAY [1..16] OF GL.GLdouble;
    viewport        : UNTRACED REF ARRAY [1..4] OF GL.GLint; 
    objx, objy, objz: GL.GLdoubleStar): Ctypes.int;

<*EXTERNAL gluScaleImage:WINAPI*>
PROCEDURE gluScaleImage (format   : GL.GLenum; 
                         widthin  : GL.GLint; 
                         heightin : GL.GLint;
                         typein   : GL.GLenum; 
                         datain   : Ctypes.void_star; 
                         widthout : GL.GLint; 
                         heightout: GL.GLint; 
                         typeout  : GL.GLenum; 
                         dataout  : Ctypes.void_star): Ctypes.int;

<*EXTERNAL gluBuild1DMipmaps:WINAPI*>
PROCEDURE gluBuild1DMipmaps (target    : GL.GLenum;
                             components: GL.GLint; 
                             width     : GL.GLint; 
                             format    : GL.GLenum; 
                             type      : GL.GLenum; 
                             data      : Ctypes.void_star): Ctypes.int;

<*EXTERNAL gluBuild2DMipmaps:WINAPI*>
PROCEDURE gluBuild2DMipmaps (target    : GL.GLenum;
                             components: GL.GLint; 
                             width     : GL.GLint; 
                             format    : GL.GLenum; 
                             type      : GL.GLenum; 
                             data      : Ctypes.void_star): Ctypes.int;

<*EXTERNAL gluNewQuadric:WINAPI*>
PROCEDURE gluNewQuadric (): GLUquadricObjStar;

<*EXTERNAL gluDeleteQuadric:WINAPI*>
PROCEDURE gluDeleteQuadric (state: GLUquadricObjStar);

<*EXTERNAL gluQuadricNormals:WINAPI*>
PROCEDURE gluQuadricNormals (qobj   : GLUquadricObjStar; 
                             normals: GL.GLenum);

<*EXTERNAL gluQuadricTexture:WINAPI*>
PROCEDURE gluQuadricTexture (qobj         : GLUquadricObjStar; 
                             textureCoords: GL.GLboolean);

<*EXTERNAL gluQuadricOrientation:WINAPI*>
PROCEDURE gluQuadricOrientation (qobj       : GLUquadricObjStar; 
                                 orientation: GL.GLenum);

<*EXTERNAL gluQuadricDrawStyle:WINAPI*>
PROCEDURE gluQuadricDrawStyle (qobj     : GLUquadricObjStar; 
                               drawStyle: GL.GLenum);

<*EXTERNAL gluCylinder:WINAPI*>
PROCEDURE gluCylinder (qobj      : GLUquadricObjStar; 
                       baseRadius: GL.GLdouble;
                       topRadius : GL.GLdouble; 
                       height    : GL.GLdouble; 
                       slices    : GL.GLint; 
                       stacks    : GL.GLint);

<*EXTERNAL gluDisk:WINAPI*>
PROCEDURE gluDisk (qobj       : GLUquadricObjStar; 
                   innerRadius: GL.GLdouble; 
                   outerRadius: GL.GLdouble;
                   slices     : GL.GLint; 
                   loops      : GL.GLint);

<*EXTERNAL gluPartialDisk:WINAPI*>
PROCEDURE gluPartialDisk (qobj       : GLUquadricObjStar; 
                          innerRadius: GL.GLdouble; 
                          outerRadius: GL.GLdouble;
                          slices     : GL.GLint; 
                          loops      : GL.GLint;
                          startAngle : GL.GLdouble; 
                          sweepAngle : GL.GLdouble);

<*EXTERNAL gluSphere:WINAPI*>
PROCEDURE gluSphere (qobj  : GLUquadricObjStar; 
                     radius: GL.GLdouble; 
                     slices: GL.GLint; 
                     stacks: GL.GLint);

<*EXTERNAL gluQuadricCallback:WINAPI*>
PROCEDURE gluQuadricCallback (qobj : GLUquadricObjStar; 
                              which: GL.GLenum;
                              fn   : GLUquadricErrorProc);

<*EXTERNAL gluNewTess:WINAPI*>
PROCEDURE gluNewTess (): GLUtriangulatorObjStar;

<*EXTERNAL gluTessCallback:WINAPI*>
PROCEDURE gluTessCallback (tobj : GLUtriangulatorObjStar; 
                           which: GL.GLenum;
                           fn   : GLUtessAnyProc);

<*EXTERNAL gluDeleteTess:WINAPI*>
PROCEDURE gluDeleteTess (tobj: GLUtriangulatorObjStar);

<*EXTERNAL gluBeginPolygon:WINAPI*>
PROCEDURE gluBeginPolygon (tobj: GLUtriangulatorObjStar);

<*EXTERNAL gluEndPolygon:WINAPI*>
PROCEDURE gluEndPolygon (tobj: GLUtriangulatorObjStar);

<*EXTERNAL gluNextContour:WINAPI*>
PROCEDURE gluNextContour (tobj: GLUtriangulatorObjStar; 
                          type: GL.GLenum);

<*EXTERNAL gluTessVertex:WINAPI*>
PROCEDURE gluTessVertex (tobj: GLUtriangulatorObjStar; 
                         v   : UNTRACED REF ARRAY [1 .. 3] OF GL.GLdouble; 
                         data: GL.GLvoidStar);

<*EXTERNAL gluNewNurbsRenderer:WINAPI*>
PROCEDURE gluNewNurbsRenderer (): GLUnurbsObjStar;

<*EXTERNAL gluDeleteNurbsRenderer:WINAPI*>
PROCEDURE gluDeleteNurbsRenderer (nobj: GLUnurbsObjStar);

<*EXTERNAL gluBeginSurface:WINAPI*>
PROCEDURE gluBeginSurface (nobj: GLUnurbsObjStar);

<*EXTERNAL gluBeginCurve:WINAPI*>
PROCEDURE gluBeginCurve (nobj: GLUnurbsObjStar);

<*EXTERNAL gluEndCurve:WINAPI*>
PROCEDURE gluEndCurve (nobj: GLUnurbsObjStar);

<*EXTERNAL gluEndSurface:WINAPI*>
PROCEDURE gluEndSurface (nobj: GLUnurbsObjStar);

<*EXTERNAL gluBeginTrim:WINAPI*>
PROCEDURE gluBeginTrim (nobj: GLUnurbsObjStar);

<*EXTERNAL gluEndTrim:WINAPI*>
PROCEDURE gluEndTrim (nobj: GLUnurbsObjStar);

<*EXTERNAL gluPwlCurve:WINAPI*>
PROCEDURE gluPwlCurve (nobj  : GLUnurbsObjStar;
                       count : GL.GLint; 
                       array : UNTRACED REF ARRAY OF GL.GLfloat; 
                       stride: GL.GLint; 
                       type  : GL.GLenum);

<*EXTERNAL gluNurbsCurve:WINAPI*>
PROCEDURE gluNurbsCurve (nobj    : GLUnurbsObjStar;
                         nknots  : GL.GLint; 
                         knot    : UNTRACED REF ARRAY OF GL.GLfloat; 
                         stride  : GL.GLint; 
                         ctlarray: UNTRACED REF ARRAY OF GL.GLfloat; 
                         order   : GL.GLint; 
                         type    : GL.GLenum);

<*EXTERNAL gluNurbsSurface:WINAPI*>
PROCEDURE gluNurbsSurface (nobj       : GLUnurbsObjStar; 
                           uknot_count: GL.GLint; 
                           uknot      : UNTRACED REF ARRAY OF GL.GLfloat; 
                           vknot_count: GL.GLint; 
                           vknot      : UNTRACED REF ARRAY OF GL.GLfloat; 
                           u_stride   : GL.GLint; 
                           v_stride   : GL.GLint; 
                           ctlarray   : UNTRACED REF ARRAY OF GL.GLfloat; 
                           uorder     : GL.GLint; 
                           vorder     : GL.GLint; 
                           type       : GL.GLenum);

<*EXTERNAL gluLoadSamplingMatrices:WINAPI*>
PROCEDURE gluLoadSamplingMatrices (
    nobj       : GLUnurbsObjStar; 
    modelMatrix: UNTRACED REF ARRAY [1..16] OF GL.GLfloat;
    projMatrix : UNTRACED REF ARRAY [1..16] OF GL.GLfloat;
    viewport   : UNTRACED REF ARRAY [1..4] OF GL.GLint);  
	
<*EXTERNAL gluNurbsProperty:WINAPI*>
PROCEDURE gluNurbsProperty (nobj    : GLUnurbsObjStar; 
                            property: GL.GLenum; 
                            value   : GL.GLfloat);

<*EXTERNAL gluGetNurbsProperty:WINAPI*>
PROCEDURE gluGetNurbsProperty (nobj    : GLUnurbsObjStar; 
                               property: GL.GLenum;
                               value   : UNTRACED REF GL.GLfloat);

<*EXTERNAL gluNurbsCallback:WINAPI*>
PROCEDURE gluNurbsCallback (nobj : GLUnurbsObjStar; 
                            which: GL.GLenum;
                            fn   : GLUnurbsErrorProc);

(**** Generic constants ****)

(* Errors: (return value 0 = no error) *)

CONST
  GLU_INVALID_ENUM  = 100900;
  GLU_INVALID_VALUE = 100901;
  GLU_OUT_OF_MEMORY = 100902;

(* For laughs: *)

  GLU_TRUE  = GL.GL_TRUE;
  GLU_FALSE = GL.GL_FALSE;


(**** Quadric constants ****)

(* Types of normals: *)

  GLU_SMOOTH = 100000;
  GLU_FLAT   = 100001;
  GLU_NONE   = 100002;

(* DrawStyle types: *)

  GLU_POINT      = 100010;
  GLU_LINE       = 100011;
  GLU_FILL       = 100012;
  GLU_SILHOUETTE = 100013;

(* Orientation types: *)

  GLU_OUTSIDE = 100020;
  GLU_INSIDE  = 100021;

(**** Tesselation constants ****)

(* Callback types: *)

  GLU_BEGIN     = 100100;	
  GLU_VERTEX    = 100101;	
  GLU_END       = 100102;	
  GLU_ERROR     = 100103;	
  GLU_EDGE_FLAG = 100104;	

(* Contours types: *)

  GLU_CW          = 100120;
  GLU_CCW         = 100121;
  GLU_INTERIOR    = 100122;
  GLU_EXTERIOR    = 100123;
  GLU_UNKNOWN     = 100124;

  GLU_TESS_ERROR1 = 100151;
  GLU_TESS_ERROR2 = 100152;
  GLU_TESS_ERROR3 = 100153;
  GLU_TESS_ERROR4 = 100154;
  GLU_TESS_ERROR5 = 100155;
  GLU_TESS_ERROR6 = 100156;
  GLU_TESS_ERROR7 = 100157;
  GLU_TESS_ERROR8 = 100158;


(**** NURBS constants ****)

(* Properties: *)

  GLU_AUTO_LOAD_MATRIX   = 100200;
  GLU_CULLING            = 100201;
  GLU_SAMPLING_TOLERANCE = 100203;
  GLU_DISPLAY_MODE       = 100204;

(* Trimming curve types *)

  GLU_MAP1_TRIM_2 = 100210;
  GLU_MAP1_TRIM_3 = 100211;

(* Display modes: *)

  GLU_OUTLINE_POLYGON = 100240;
  GLU_OUTLINE_PATCH   = 100241;

(* Errors: *)

  GLU_NURBS_ERROR1  = 100251;
  GLU_NURBS_ERROR2  = 100252;
  GLU_NURBS_ERROR3  = 100253;
  GLU_NURBS_ERROR4  = 100254;
  GLU_NURBS_ERROR5  = 100255;
  GLU_NURBS_ERROR6  = 100256;
  GLU_NURBS_ERROR7  = 100257;
  GLU_NURBS_ERROR8  = 100258;
  GLU_NURBS_ERROR9  = 100259;
  GLU_NURBS_ERROR10 = 100260;
  GLU_NURBS_ERROR11 = 100261;
  GLU_NURBS_ERROR12 = 100262;
  GLU_NURBS_ERROR13 = 100263;
  GLU_NURBS_ERROR14 = 100264;
  GLU_NURBS_ERROR15 = 100265;
  GLU_NURBS_ERROR16 = 100266;
  GLU_NURBS_ERROR17 = 100267;
  GLU_NURBS_ERROR18 = 100268;
  GLU_NURBS_ERROR19 = 100269;
  GLU_NURBS_ERROR20 = 100270;
  GLU_NURBS_ERROR21 = 100271;
  GLU_NURBS_ERROR22 = 100272;
  GLU_NURBS_ERROR23 = 100273;
  GLU_NURBS_ERROR24 = 100274;
  GLU_NURBS_ERROR25 = 100275;
  GLU_NURBS_ERROR26 = 100276;
  GLU_NURBS_ERROR27 = 100277;
  GLU_NURBS_ERROR28 = 100278;
  GLU_NURBS_ERROR29 = 100279;
  GLU_NURBS_ERROR30 = 100280;
  GLU_NURBS_ERROR31 = 100281;
  GLU_NURBS_ERROR32 = 100282;
  GLU_NURBS_ERROR33 = 100283;
  GLU_NURBS_ERROR34 = 100284;
  GLU_NURBS_ERROR35 = 100285;
  GLU_NURBS_ERROR36 = 100286;
  GLU_NURBS_ERROR37 = 100287;

END GLu.
