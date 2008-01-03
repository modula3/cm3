(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 18 10:13:50 PST 1994 by najork                   *)
(*       Created on Thu Feb 17 19:21:28 PST 1994 by najork                   *)


(*****************************************************************************)
(* Some selected X extensions                                                *)
(* based on /usr/include/X11/extensions/multibuf.h                           *)
(*****************************************************************************)

UNSAFE INTERFACE Xmbuf;

IMPORT Ctypes, X;

FROM Ctypes IMPORT char, int, int_star, long_int, short, 
                   unsigned_int, unsigned_long;

CONST
  X_MbufGetBufferVersion         = 0;
  X_MbufCreateImageBuffers       = 1;
  X_MbufDestroyImageBuffers      = 2;
  X_MbufDisplayImageBuffers      = 3;
  X_MbufSetMultiBufferAttributes = 4;
  X_MbufGetMultiBufferAttributes = 5;
  X_MbufSetBufferAttributes      = 6;
  X_MbufGetBufferAttributes      = 7;
  X_MbufGetBufferInfo            = 8;
  X_MbufCreateStereoWindow       = 9;
  X_MbufClearImageBufferArea     = 10;

(*
 * update_action field
 *)

CONST
  MultibufferActionUndefined  = 0;
  MultibufferActionBackground = 1;
  MultibufferActionUntouched  = 2;
  MultibufferActionCopied     = 3;

(*
 * update_hint field
 *)

  MultibufferHintFrequent     = 0;
  MultibufferHintIntermittent = 1;
  MultibufferHintStatic       = 2;

(*
 * valuemask fields
 *)

  MultibufferWindowUpdateHint : long_int = 1;
  MultibufferBufferEventMask  : long_int = 1;

(*
 * mono vs. stereo and left vs. right
 *)

  MultibufferModeMono   = 0;
  MultibufferModeStereo = 1;

  MultibufferSideMono   = 0;
  MultibufferSideLeft   = 1;
  MultibufferSideRight  = 2;

(*
 * clobber state
 *)

  MultibufferUnclobbered        = 0;
  MultibufferPartiallyClobbered = 1;
  MultibufferFullyClobbered     = 2;

(*
 * event stuff
 *)

  MultibufferClobberNotifyMask = 16_02000000;
  MultibufferUpdateNotifyMask  = 16_04000000;

  MultibufferClobberNotify  = 0;
  MultibufferUpdateNotify   = 1;
  MultibufferNumberOfEvents = MultibufferUpdateNotify + 1;

  MultibufferBadBuffer      = 0;
  MultibufferNumberOfErrors = MultibufferBadBuffer + 1;

(*
 * per-screen buffer info (there will be lists of them)
 *)

TYPE
  XmbufBufferInfo = RECORD
    visualid    : X.VisualID;   (* visual usuable at this depth *)
    max_buffers : short;        (* most buffers for this visual *)
    depth       : char;          (* depth of buffers to be created *)
    pad         : char;
  END;
  XmbufBufferInfoStar     = UNTRACED REF XmbufBufferInfo;
  XmbufBufferInfoStarStar = UNTRACED REF XmbufBufferInfoStar;

(*
 * Extra definitions that will only be needed in the client
 *)

  Multibuffer = X.XID;
  MultibufferStar = UNTRACED REF Multibuffer;

  XmbufClobberNotifyEvent = RECORD
    type       : int;            (* of event *)
    serial     : unsigned_long;  (* # of last request processed by server *)
    send_event : int;         (* true if this came frome a SendEvent request *)
    display    : X.DisplayStar;  (* Display the event was read from *)
    buffer     : Multibuffer;    (* buffer of event *)
    state      : int;            (* see Clobbered constants above *)
  END;

  XmbufUpdateNotifyEvent = RECORD
    type       : int;            (* of event *)
    serial     : unsigned_long;  (* # of last request processed by server *)
    send_event : int;         (* true if this came frome a SendEvent request *)
    display    : X.DisplayStar;  (* Display the event was read from *)
    buffer     : Multibuffer;    (* buffer of event *)
  END;

(*
 * per-window attributes that can be got
 *)

  XmbufWindowAttributes = RECORD
    displayed_index : int;    (* which buffer is being displayed *)
    update_action   : int;    (* Undefined, Background, Untouched, Copied *)
    update_hint     : int;    (* Frequent, Intermittent, Static *)
    window_mode     : int;    (* Mono, Stereo *)
  END;
  XmbufWindowAttributesStar = UNTRACED REF XmbufWindowAttributes;

(*
 * per-window attributes that can be set
 *)

  XmbufSetWindowAttributes = RECORD
    update_hint : int;        (* Frequent, Intermittent, Static *)
  END;
  XmbufSetWindowAttributesStar = UNTRACED REF XmbufSetWindowAttributes;


(*
 * per-buffer attributes that can be got
 *)

  XmbufBufferAttributes = RECORD
    window       : X.Window;         (* which window this belongs to *)
    event_mask   : unsigned_long;    (* events that have been selected *)
    buffer_index : int;              (* which buffer is this *)
    side         : int;              (* Mono, Left, Right *)
  END;
  XmbufBufferAttributesStar = UNTRACED REF XmbufBufferAttributes;

(*
 * per-buffer attributes that can be set
 *)

  XmbufSetBufferAttributes = RECORD
    event_mask : unsigned_long;      (* events that have been selected *)
  END;
  XmbufSetBufferAttributesStar = UNTRACED REF XmbufSetBufferAttributes;

(*** is extension on server? ***)

<* EXTERNAL *> PROCEDURE XmbufQueryExtension (
                    display           : X.DisplayStar;
                    event_base_return : int_star;
                    error_base_return : int_star) : X.Bool;


(*** what is extension rev ***)

<* EXTERNAL *> PROCEDURE XmbufGetVersion (
                    display              : X.DisplayStar;
                    major_version_return : int_star;
                    minor_version_return : int_star) : X.Status;



(*** create buffers for window ***)

<* EXTERNAL *> PROCEDURE XmbufCreateBuffers (
                    display      : X.DisplayStar;
                    window       : X.Window;
                    count        : Ctypes.int;
                    updateAction : Ctypes.int;
                    updateHint   : Ctypes.int;
                    xmbBuffers   : MultibufferStar) : Ctypes.int;


(*** destroy buffers for window ***)

<* EXTERNAL *> PROCEDURE XmbufDestroyBuffers (
                    display : X.DisplayStar;
                    window  : X.Window);

(*** make buffers current ***)

<* EXTERNAL *> PROCEDURE XmbufDisplayBuffers (
                    display   : X.DisplayStar;
                    count     : Ctypes.int;
                    xmbBuffer : MultibufferStar;
                    min_delay : Ctypes.int;
                    max_delay : Ctypes.int);	


(*** get extension window attrs ***)

<* EXTERNAL *> PROCEDURE XmbufGetWindowAttributes (
                    display    : X.DisplayStar;
                    window     : X.Window;
                    attributes : XmbufWindowAttributesStar) : X.Status;

(*** set extension window attrs ***)

<* EXTERNAL *> PROCEDURE XmbufChangeWindowAttributes (
                    display    : X.DisplayStar;
                    window     : X.Window;
                    valuemask  : unsigned_long;
                    attributes : XmbufSetWindowAttributesStar);

(*** get buffer attrs ***)

<* EXTERNAL *> PROCEDURE XmbufGetBufferAttributes(
                    display    : X.DisplayStar;
                    buffer     : Multibuffer;
                    attributes : XmbufBufferAttributesStar) : X.Status;

(*** set buffer attrs ***)

<* EXTERNAL *> PROCEDURE XmbufChangeBufferAttributes (
                    display    : X.DisplayStar;
                    buffer     : Multibuffer;
                    valuemask  : unsigned_long;
                    attributes : XmbufSetBufferAttributesStar);

(*** get mono and stereo parameters ***)

<* EXTERNAL *> PROCEDURE XmbufGetScreenInfo (
                    display            : X.DisplayStar;
                    drawable           : X.Drawable;
                    nmono_return       : int_star;
                    mono_info_return   : XmbufBufferInfoStarStar;
                    nstereo_return     : int_star;
                    stereo_info_return : XmbufBufferInfoStarStar) : X.Status;

(*** stereo version of XCreateWindow ***)

<* EXTERNAL *> PROCEDURE XmbufCreateStereoWindow(
                    display      : X.DisplayStar;
                    parent       : X.Window;
                    x            : int;
                    y            : int;
                    width        : unsigned_int;
                    height       : unsigned_int;
                    border_width : unsigned_int;
                    depth        : int;
                    class        : unsigned_int; (* InputOutput, InputOnly *)
                    visual       : X.VisualStar; 
                    valuemask    : unsigned_long;
                    attributes   : X.XSetWindowAttributes;
                    left_return  : MultibufferStar;
                    right_return : MultibufferStar) : X.Window;

END Xmbuf.
