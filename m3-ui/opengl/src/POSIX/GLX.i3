(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug 22 11:49:03 PDT 1995 by najork                   *)
(*       Created on Fri Aug 19 18:00:36 PDT 1994 by najork                   *)


UNSAFE INTERFACE GLX;

IMPORT Ctypes, GL, X;

(*****************************************************************************)
(* Types and Constants (complete)                                            *)
(*****************************************************************************)

TYPE
  GLXContextID = X.XID;
  GLXPixmap    = X.XID;
  GLXDrawable  = X.XID;
  GLXContext   = ADDRESS;

(* Names for attributes to glXGetConfig. *)

CONST
  GLX_USE_GL           =  1;       (* support GLX rendering *)
  GLX_BUFFER_SIZE      =  2;       (* depth of the color buffer *)
  GLX_LEVEL            =  3;       (* level in plane stacking *)
  GLX_RGBA             =  4;       (* true if RGBA mode *)
  GLX_DOUBLEBUFFER     =  5;       (* double buffering supported *)
  GLX_STEREO           =  6;       (* stereo buffering supported *)
  GLX_AUX_BUFFERS      =  7;       (* number of aux buffers *)
  GLX_RED_SIZE         =  8;       (* number of red component bits *)
  GLX_GREEN_SIZE       =  9;       (* number of green component bits *)
  GLX_BLUE_SIZE        = 10;       (* number of blue component bits *)
  GLX_ALPHA_SIZE       = 11;       (* number of alpha component bits *)
  GLX_DEPTH_SIZE       = 12;       (* number of depth bits *)
  GLX_STENCIL_SIZE     = 13;       (* number of stencil bits *)
  GLX_ACCUM_RED_SIZE   = 14;       (* number of red accum bits *)
  GLX_ACCUM_GREEN_SIZE = 15;       (* number of green accum bits *)
  GLX_ACCUM_BLUE_SIZE  = 16;       (* number of blue accum bits *)
  GLX_ACCUM_ALPHA_SIZE = 17;       (* number of alpha accum bits *)


(* Error return values from glXGetConfig.  
   Success is indicated by a value of 0. *)

CONST
  GLX_BAD_SCREEN    =  1;       (* screen # is bad *)
  GLX_BAD_ATTRIBUTE =  2;       (* attribute to get is bad *)
  GLX_NO_EXTENSION  =  3;       (* no glx extension on server *)
  GLX_BAD_VISUAL    =  4;       (* visual # not known by GLX *)

(*****************************************************************************)
(* Procedures (not yet complete; 7 out of 17 functions)                      *)
(*****************************************************************************)

<*EXTERNAL*> PROCEDURE glXChooseVisual (
                               dpy        : X.DisplayStar; 
                               screen     : Ctypes.int; 
                               attribList : UNTRACED REF ARRAY OF Ctypes.int
                         ) : X.XVisualInfoStar;

<*EXTERNAL*> PROCEDURE glXCopyContext (dpy : X.DisplayStar; 
                                       src : GLXContext; 
                                       dst : GLXContext; 
                                       mask: GL.GLuint);

<*EXTERNAL*> PROCEDURE glXCreateContext (dpy      : X.DisplayStar;
                                         vis      : X.XVisualInfoStar;
                                         shareList: GLXContext; 
                                         direct   : X.Bool) : GLXContext;

<*EXTERNAL*> PROCEDURE glXCreateGLXPixmap (dpy   : X.DisplayStar;
                                           vis   : X.XVisualInfoStar;
                                           pixmap: X.Pixmap): GLXPixmap;

<*EXTERNAL*> PROCEDURE glXDestroyContext (dpy: X.DisplayStar; 
                                          ctx: GLXContext);

<*EXTERNAL*> PROCEDURE glXDestroyGLXPixmap (dpy: X.DisplayStar; 
                                            pix: GLXPixmap);

<*EXTERNAL*> PROCEDURE glXGetConfig (dpy   : X.DisplayStar;
                                     vis   : X.XVisualInfoStar;
                                     attrib: Ctypes.int; 
                           (* OUT *) value : Ctypes.int_star): Ctypes.int;

<*EXTERNAL*> PROCEDURE glXGetCurrentContext (): GLXContext;

<*EXTERNAL*> PROCEDURE glXGetCurrentDrawable (): GLXDrawable;

<*EXTERNAL*> PROCEDURE glXIsDirect (dpy: X.DisplayStar; 
                                    ctx: GLXContext): X.Bool;

<*EXTERNAL*> PROCEDURE glXMakeCurrent (dpy     : X.DisplayStar; 
                                       drawable: GLXDrawable; 
                                       ctx     : GLXContext): X.Bool;

<*EXTERNAL*> PROCEDURE glXQueryExtension (
                              dpy       : X.DisplayStar; 
                      (*OUT*) errorBase : Ctypes.int_star;
                      (*OUT*) eventBase : Ctypes.int_star) : X.Bool;

<*EXTERNAL*> PROCEDURE glXQueryVersion (
                              dpy  : X.DisplayStar; 
                      (*OUT*) major: Ctypes.int_star; 
                      (*OUT*) minor: Ctypes.int_star): X.Bool;

<*EXTERNAL*> PROCEDURE glXSwapBuffers (dpy     : X.DisplayStar; 
                                       drawable: GLXDrawable);

<*EXTERNAL*> PROCEDURE glXUseXFont (font    : X.Font; 
                                    first   : Ctypes.int; 
                                    count   : Ctypes.int; 
                                    listBase: Ctypes.int);

<*EXTERNAL*> PROCEDURE glXWaitGL ();

<*EXTERNAL*> PROCEDURE glXWaitX ();

END GLX.
