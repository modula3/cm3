/* Copyright (C) 1993, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Tue Nov  1 09:14:56 PST 1994 by kalsow  */

/* Exception handling from "Perry" via Chris Kent. */
/* It is fragile. Change no block structure or statements around it. */

#include <stdio.h>
#include <errno.h>
#include "Xlib.h"
#include "Xatom.h"
#include "XDPSlib.h"
#include "cursorfont.h"
#include <dpsXclient.h>
#include <dpsexcept.h>
#include <strings.h>
#include "dpsfriends.h"
#include "wraps.h"

extern int errno;

#define FullWidth 1024   /* Congruent to definition in DPS.i3 */
#define FullHeight 1024  /* Congruent to definition in DPS.i3 */

#define GreyRampCount 16  /* Full grey = this many values */

/* Code that calls the wrap and computes with transforms. */
/* Copied and corrected (!) from page 4-3 of the Guide. */

/* This struct is the C manifestation of a DPS.T! */
struct t 
    {
    char *PointerToMethods;
    Display *dpy;
    Window win;
    int fd;
    DPSContext ctx;
    GC gc;
    Cursor cursor;
    int xWidth;
    int xHeight;
    float ctm[6];
    float invctm[6];
    int xoffset;
    int yoffset;
    int planes;
    };

void xtodps (ctx, x, y, ux, uy)
DPSContext ctx;
int x, y;
float *ux, *uy;
{
  float ctm[6], invctm[6];
  int xOffset, yOffset;
  float xx, yy;

  GetTransformWrap (ctx, ctm, invctm, &yOffset, &xOffset);
  /* Notice the swapped order ^^ of x and y offsets. Bogosity in example wrap! */

  x -= xOffset;
  y -= yOffset;

  xx = x; 
  yy = y;

  *ux = invctm[0] * xx + invctm[2] * yy + invctm[4];
  *uy = invctm[1] * xx + invctm[3] * yy + invctm[5];
}

int xyupathhit (ctx, x, y, u)
DPSContext ctx;
float x, y;
char *u;
{
 int result;

 XYHitWrap (ctx, x, y, u, &result);

 return result;
}

void dpstox (ctx, ux, uy, x, y)
DPSContext ctx;
int *x, *y;
float ux, uy;
{
 float ctm[6], invctm[6];
 int xOffset, yOffset;

  GetTransformWrap (ctx, ctm, invctm, &yOffset, &xOffset);
  /* Notice the swapped order ^^ of x and y offsets. Bogosity in example wrap! */
  *x = (ctm[0] * ux + ctm[2] * uy + ctm[4]) + xOffset;
  *y = (ctm[1] * ux + ctm[3] * uy + ctm[5]) + yOffset;
}

static void HandleStatus(ctx, status)
DPSContext ctx;
int status;
{
  char *ptr;
  switch (status) 
    {
    case PSRUNNING:           ptr = "running"; break;
    case PSNEEDSINPUT:        ptr = "needs input"; break;
    case PSZOMBIE:            ptr = "zombie"; break;
    case PSFROZEN:            ptr = "frozen"; break;
    default:                  ptr = "unknown status"; break;
    }
  fprintf(stderr, "[Status event - %s]\n", ptr);
  fflush(stderr);
  if (status == PSFROZEN) 
    {
    fprintf(stderr, "Attempting an XDPSUnfreezeContext.\n");
    fflush(stderr);
    XDPSUnfreezeContext(ctx);
    }
}

static void TextOut(ctx, buffer, count)
DPSContext ctx;
char *buffer;
unsigned count;
{
  fwrite(buffer, 1, count, stdout);
  fflush(stdout);
}

void InitializePostScript (w, height)
struct t *w;
int height;
{
 GC defaultGC;
 Visual *visual;  /* I have no idea why this is a pointer type. */
 XStandardColormap grayramp;
 XStandardColormap ccube;
 int actual;

  defaultGC = DefaultGC(w->dpy, 0);
  w->gc = XCreateGC (w->dpy, w->win, 0, NULL);
  XCopyGC (w->dpy, defaultGC, 8388607, w->gc);

  if ( w->planes == 256 )
    {
    w->ctx = XDPSCreateSimpleContext ( w->dpy, w->win, w->gc, 0, height,
      TextOut, DPSDefaultErrorProc, NULL );
    }
  else if ( w->planes == 255 )
    {
    visual = DefaultVisual (w->dpy, 0);

    XGetStandardColormap (w->dpy, DefaultRootWindow(w->dpy), &grayramp, XA_RGB_DEFAULT_MAP);
    grayramp.colormap = XCreateColormap (w->dpy, DefaultRootWindow(w->dpy), visual, AllocAll);
    grayramp.red_max = GreyRampCount - 1;
    grayramp.base_pixel = 2; /* suggested: BlackPixel */
    grayramp.red_mult = 1;   /* suggested: WhitePixel - BlackPixel */

    ccube.colormap = XCreateColormap (w->dpy, DefaultRootWindow(w->dpy), visual, AllocAll);
    ccube.red_max = -1; ccube.red_mult = 1;
    ccube.green_max = -1; ccube.green_mult = 1;
    ccube.blue_max = -1; ccube.blue_mult = 1;

    actual = 0; /* Specification does not say what this is! */
    w->ctx = XDPSCreateContext ( w->dpy, w->win, w->gc, 0, height,
      0, &grayramp, &ccube, actual,
      TextOut, DPSDefaultErrorProc, NULL );
    }
  else
    {
    visual = DefaultVisual (w->dpy, 0);

    XGetStandardColormap (w->dpy, DefaultRootWindow(w->dpy), &grayramp, XA_RGB_DEFAULT_MAP);
    grayramp.colormap = XCreateColormap (w->dpy, DefaultRootWindow(w->dpy), visual, AllocAll);
    grayramp.red_max = 1;
    if ( w->planes == 1 ) {grayramp.base_pixel = 0;}
     else if ( w->planes == 2 ) {grayramp.base_pixel = 1;}
     else if ( w->planes == 4 ) {grayramp.base_pixel = 3;}
     else 
      {
      fprintf (stderr, "Bogus value for -planes- in InitializePostScript: %d.\n", w->planes);
      grayramp.base_pixel = 0;
      }
    grayramp.red_mult = 1;  

    ccube.colormap = XCreateColormap (w->dpy, DefaultRootWindow(w->dpy), visual, AllocAll);
    ccube.red_max = -1; ccube.red_mult = 1;
    ccube.green_max = -1; ccube.green_mult = 1;
    ccube.blue_max = -1; ccube.blue_mult = 1;

    actual = 0; /* Specification does not say what this is! */
    w->ctx = XDPSCreateContext ( w->dpy, w->win, w->gc, 0, height,
      0, &grayramp, &ccube, actual,
      TextOut, DPSDefaultErrorProc, NULL );

    XSetPlaneMask (w->dpy, w->gc, w->planes);
    }

  if (w->ctx == NULL) 
    {
    printf("Server does not have PostScript extension.");
    exit(1);
    }

  XDPSRegisterStatusProc(w->ctx, HandleStatus);
  dpsWritePostScript (w->ctx, "resyncstart\n");

  dpsWritePostScript (w->ctx, " 5000 dict begin ");
}

void stufftransforms (win)
struct t *win;
{
  int xOffset, yOffset;

  GetTransformWrap (win->ctx, win->ctm, win->invctm, &yOffset, &xOffset);
  /* Notice the swapped order ^^ of x and y offsets. Bogosity in example wrap! */
  win->xoffset = xOffset;
  win->yoffset = yOffset;
}

void transformtodps (win, x, y, ux, uy)
struct t *win;
int x, y;
float *ux, *uy;
{
  float xx, yy;

  x -= win->xoffset;
  y -= win->yoffset;

  xx = x; 
  yy = y;

  *ux = win->invctm[0] * xx + win->invctm[2] * yy + win->invctm[4];
  *uy = win->invctm[1] * xx + win->invctm[3] * yy + win->invctm[5];
}

doinitialize()
{
}

noticeGC (w)
struct t *w;
{ 
  XDrawPoint (w->dpy, w->win, w->gc, 0, 0);
}

noticeCursor (w)
struct t *w;
{ 
 XColor fore;
 XColor back;

  if (w->cursor == NULL) 
    {
    fprintf (stderr, "Window has no cursor in noticeCursor!\n");
    return;
    }
  fore.pixel = 0; fore.red = 65535; fore.green = 65535; fore.blue = 65535; 
  if ( w->planes == 1 ) 
    { back.pixel = 0; back.red = 65535; back.green = 65535; back.blue = 0; }
  else if ( w->planes == 2 ) 
    { back.pixel = 0; back.red = 65535; back.green = 0; back.blue = 65535; }
  else if ( w->planes == 4 ) 
    { back.pixel = 0; back.red = 0; back.green = 65535; back.blue = 65535; }
  else 
    { 
    fprintf (stderr, "Bad -planes- value in noticeCursor = %d!\n", w->planes);
    back.pixel = 0; back.red = 0; back.green = 0; back.blue = 0; 
    }

 XRecolorCursor (w->dpy, w->cursor, &fore, &back);
 XDefineCursor (w->dpy, w->win, w->cursor);
}

docreatesimplewindow (w, width, height)
int width;
int height;
struct t *w;
{ 
 Colormap colormap;
 Visual *visual;  /* I have no idea why this is a pointer type. */
 int newWindow;
 XSetWindowAttributes xswa;

  newWindow = w->win == 0;

  if ( w->dpy == 0 )
    {
    w->dpy = XOpenDisplay("");
     if (w->dpy == NULL) 
       {
       fprintf (stderr, "Can't open display!\n");
       exit (1);
       }
    w->fd = XConnectionNumber(w->dpy);
    }

  if ( newWindow )
    {
    w->win = XCreateSimpleWindow ( w->dpy, 
      DefaultRootWindow(w->dpy), 0, 0, FullWidth, FullHeight, 1, 
      BlackPixel(w->dpy, DefaultScreen(w->dpy)), WhitePixel(w->dpy, DefaultScreen(w->dpy)) );

    XSelectInput ( w->dpy, w->win, 
      ExposureMask | StructureNotifyMask 
      | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask
      | KeyPressMask | KeyReleaseMask );

    xswa.bit_gravity = SouthWestGravity;
    xswa.bit_gravity = NorthWestGravity; 
    xswa.backing_store = 0 /*Never*/ /*WhenMapped*/;
    XChangeWindowAttributes(w->dpy, w->win, CWBitGravity | CWBackingStore, &xswa);

    XStoreName(w->dpy, w->win, "");
    XSetIconName(w->dpy, w->win, "DPS");
    }

  InitializePostScript (w, FullHeight);

  if ( newWindow )
    {
    XResizeWindow ( w->dpy, w->win, width, height );
    XMapWindow(w->dpy, w->win);
    }

  if ( w->planes == 255 )
    {
    visual = DefaultVisual (w->dpy, 0);
    colormap = XCreateColormap (w->dpy, DefaultRootWindow(w->dpy), visual, AllocAll);
    fillGreyRamp (w->dpy, colormap, 2, 2 + GreyRampCount - 1);
    XSetWindowColormap ( w->dpy, w->win, colormap );
    }
   else if ( w->planes < 255 )
    {
    visual = DefaultVisual (w->dpy, 0);
    colormap = XCreateColormap (w->dpy, DefaultRootWindow(w->dpy), visual, AllocAll);
    stuffColor (w->dpy, colormap, 0, 0,     0,     0);
    stuffColor (w->dpy, colormap, 1, 0,     0,     65535);
    stuffColor (w->dpy, colormap, 2, 0,     65535, 0);
    stuffColor (w->dpy, colormap, 3, 0,     65535, 65535);
    stuffColor (w->dpy, colormap, 4, 65535, 0,     0);
    stuffColor (w->dpy, colormap, 5, 65535, 0,     65535);
    stuffColor (w->dpy, colormap, 6, 65535, 65535, 0);
    stuffColor (w->dpy, colormap, 7, 65535, 65535, 65535);
    XSetWindowColormap ( w->dpy, w->win, colormap );
    }

  if ( newWindow && (w->planes<255) )
    {
    w->cursor = XCreateFontCursor (w->dpy, XC_left_ptr);
    noticeCursor (w);
    }

  if ( newWindow )
    {
    XSaveContext (w->dpy, w->win, 1, w->ctx);
    }
}

fillGreyRamp (dpy, colormap, from, thru)
Display *dpy;
Colormap colormap;    
int from;
int thru;
{
 int index;
 int value;
    if ( thru > 254 ) {thru = 254;}
    stuffColor (dpy, colormap, 0, 0, 0, 0);
    stuffColor (dpy, colormap, 1, 65535, 65535, 65535);
    index = from;
    while ( index <= thru ) 
      {
      value = 65535 * (index-from) / (thru-from);
      stuffColor (dpy, colormap, index, value, value, value);
      index = index + 1;
      }
}

stuffColor (dpy, colormap, index, red, green, blue)
Display *dpy;
Colormap colormap;    
int index;
int red;
int green;
int blue;
{
 XColor pixel;
    pixel.pixel = index;
    XQueryColor ( dpy, colormap, &pixel );
    pixel.red = red; pixel.green = green; pixel.blue = blue; 
    XStoreColor ( dpy, colormap, &pixel );
}

int doprocessinputs (dpy, win, event, but, modifiers, x, y, w, h)
int dpy;    
int *win;    
int *event;    
int *but;    
int *modifiers;    
int *x;    
int *y;    
int *w;    
int *h;    
{
 XEvent ev;
 DPSContext ctx;

  DURING
    /* XFlush(dpy); */
    *win = 0;     /* the nothing to do indication */
    while ( XPending (dpy) > 0 ) 
      {
      XNextEvent (dpy, &ev);
      if (XFindContext (dpy, ev.xbutton.window, 1, &ctx) != 0) 
        { fprintf (stderr, "Could not retrieve context from window\n");   
        return;
        }    
      if (ev.type == Expose) 
        {    
        *x = ev.xexpose.x;
        *y = ev.xexpose.y;
        *w = ev.xexpose.width;
        *h = ev.xexpose.height;
        *event = 99;
        *win = ev.xexpose.window;
        return;
        }    
      else if (ev.type == ButtonPress) 
        {    
        *x = ev.xbutton.x;
        *y = ev.xbutton.y;
        *event = 21;
        *but = ev.xbutton.button;
        *modifiers = ev.xbutton.state;
        *win = ev.xbutton.window;
        return;
        }    
      else if (ev.type == ButtonRelease) 
        {    
        *x = ev.xbutton.x;
        *y = ev.xbutton.y;
        *event = 22;
        *but = ev.xbutton.button;
        *modifiers = ev.xbutton.state;
        *win = ev.xbutton.window;
        return;
        }    
      else if (ev.type == MotionNotify) 
        {    
        *x = ev.xmotion.x;
        *y = ev.xbutton.y;
        *event = 23;
        *but = ev.xmotion.state; /* Notice different meaning for -but- */
        *modifiers = ev.xmotion.state;
        *win = ev.xbutton.window;
        return;
        }    
      else if (ev.type == KeyPress) 
        {    
        *but = ev.xkey.keycode;
        *modifiers = ev.xkey.state;
        *event = 31;
        *win = ev.xkey.window;
        return;
        }    
      else if (ev.type == KeyRelease) 
        {    
        *but = ev.xkey.keycode;
        *modifiers = ev.xkey.state;
        *event = 32;
        *win = ev.xkey.window;
        return;
        }    
      else if (ev.type == ConfigureNotify) 
        {    
        *event = 41;
        *win = ev.xconfigure.window;
        *x = ev.xconfigure.x;
        *y = ev.xconfigure.y;
        *w = ev.xconfigure.width;
        *h = ev.xconfigure.height;
        return;
        }    
      else /* An uninteresting Xevent. */
        {    
        }    
      } /* of while XPending */
  HANDLER
    if (Exception.Code == dps_err_ps && (DPSContext) Exception.Message == ctx)
      {
      fprintf(stderr, "[error detected processing inputs; resetting context.]\n");
      DPSResetContext(ctx);
      } else {
      /* whoa, something seriously bogus is happening! */
      fprintf(stderr, "[Ignoring bogus exception %d.]\n", Exception.Code);
      }
  END_HANDLER
    *but = 0;
    *event = 0;
    *win = 0;
    return;
} 

dpsWritePostScript (ctx, buf)
DPSContext ctx;    
char *buf;
{
  DURING
    DPSWritePostScript(ctx, buf, strlen(buf));
  HANDLER
    if (Exception.Code == dps_err_ps && (DPSContext) Exception.Message == ctx) 
      {
      fprintf(stderr, "[error detected in send; resetting context.]\n");
      DPSResetContext(ctx);
      } else {
      /* whoa, something seriously bogus is happening! */
      fprintf(stderr, "[Ignoring exception %d in dpsWritePostScript.]\n", Exception.Code);
      }
  END_HANDLER
}

/* This procedure doesn't work as one would expect, because */
/* the DPSFlushContext doesn't guarante that everything makes it */
/* through the PostScript engine.  You can still get your error */
/* complaint on a subsequent call. */
int dosendpsnervously (win, buf)
struct t *win;    
char *buf;
{
 GC defaultGC;
 GC gc;
 int returnvalue;
  returnvalue = 0;
  DURING
    noticeGC (win); 
    fprintf(stderr, "Doing DPSWritePostScript with %s\n", buf);
    DPSWritePostScript(win->ctx, buf, strlen(buf));
    fprintf(stderr, "Doing DPSFlushContext\n");
    DPSFlushContext(win->ctx);
    fprintf(stderr, "Back from DPSFlushContext\n");
    returnvalue = 1;
  HANDLER
    fprintf(stderr, "In dosendnervously handler\n");
    if (Exception.Code == dps_err_ps && (DPSContext) Exception.Message == win->ctx)
      {
      fprintf(stderr, "[error detected in dosendps; resetting context.]\n");
      DPSResetContext(win->ctx);
      } else 
      {
      /* whoa, something seriously bogus is happening! */
      fprintf(stderr,  "[Ignoring exception %d in dosendnervously.]\n", Exception.Code);
      }
  END_HANDLER
  fprintf(stderr, "At return %d\n", returnvalue);
  return returnvalue;
}

dosendps (win, buf)
struct t *win;    
char *buf;
{
 GC defaultGC;
 GC gc;
  DURING
    noticeGC (win); 
    DPSWritePostScript(win->ctx, buf, strlen(buf));
  HANDLER
    if (Exception.Code == dps_err_ps && (DPSContext) Exception.Message == win->ctx)
      {
      fprintf(stderr, "[error detected in dosendps; resetting context.]\n");
      DPSResetContext(win->ctx);
      } else 
      {
      /* whoa, something seriously bogus is happening! */
      fprintf(stderr,  "\[Ignoring exception %d in dosendps.]\n", Exception.Code);
      }
  END_HANDLER
}

doflush (win)
struct t *win;    
{
  DURING
    DPSFlushContext(win->ctx);
  HANDLER
    if (Exception.Code == dps_err_ps && (DPSContext) Exception.Message == win->ctx)
      {
      fprintf(stderr, "[error detected in flush; resetting context.]\n");
      DPSResetContext(win->ctx);
      } else {
      /* whoa, something seriously bogus is happening! */
      fprintf(stderr,  "[Ignoring exception %d in doflush.]\n", Exception.Code);
      }
  END_HANDLER
}

dowait (win)
struct t *win;    
{
  DURING
    DPSWaitContext(win->ctx);
  HANDLER
    if (Exception.Code == dps_err_ps && (DPSContext) Exception.Message == win->ctx)
      {
      fprintf(stderr, "[error detected in wait; resetting context.]\n");
      DPSResetContext(win->ctx);
      } else {
      /* whoa, something seriously bogus is happening! */
      fprintf(stderr, "[Ignoring bogus exception %d.]\n", Exception.Code);
      }
  END_HANDLER
}




