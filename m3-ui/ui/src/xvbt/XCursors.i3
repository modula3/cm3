(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Mon Feb 24 13:59:45 PST 1992 by muller *)
(* modified on Fri Jun 7 17:02:23 PDT 1991 by gnelson *)
(* modified on Thu Sep 6 21:11:01 PDT 1990 by msm *)
(* modified on Fri Aug 3 10:13:01 PDT 1990 by steveg *)
<*PRAGMA LL*>

INTERFACE XCursors;

(* Names[i] is the name of cursor (2*i,2*i+1) in the standard X cursor
   font. *)

CONST
  Names = ARRAY OF
            TEXT{
            "XC_X_cursor", "XC_arrow", "XC_based_arrow_down",
            "XC_based_arrow_up", "XC_boat", "XC_bogosity",
            "XC_bottom_left_corner", "XC_bottom_right_corner",
            "XC_bottom_side", "XC_bottom_tee", "XC_box_spiral",
            "XC_center_ptr", "XC_circle", "XC_clock", "XC_coffee_mug",
            "XC_cross", "XC_cross_reverse", "XC_crosshair",
            "XC_diamond_cross", "XC_dot", "XC_dotbox", "XC_double_arrow",
            "XC_draft_large", "XC_draft_small", "XC_draped_box",
            "XC_exchange", "XC_fleur", "XC_gobbler", "XC_gumby",
            "XC_hand1", "XC_hand2", "XC_heart", "XC_icon", "XC_iron_cross",
            "XC_left_ptr", "XC_left_side", "XC_left_tee", "XC_leftbutton",
            "XC_ll_angle", "XC_lr_angle", "XC_man", "XC_middlebutton",
            "XC_mouse", "XC_pencil", "XC_pirate", "XC_plus",
            "XC_question_arrow", "XC_right_ptr", "XC_right_side",
            "XC_right_tee", "XC_rightbutton", "XC_rtl_logo", "XC_sailboat",
            "XC_sb_down_arrow", "XC_sb_h_double_arrow", "XC_sb_left_arrow",
            "XC_sb_right_arrow", "XC_sb_up_arrow", "XC_sb_v_double_arrow",
            "XC_shuttle", "XC_sizing", "XC_spider", "XC_spraycan",
            "XC_star", "XC_target", "XC_tcross", "XC_top_left_arrow",
            "XC_top_left_corner", "XC_top_right_corner", "XC_top_side",
            "XC_top_tee", "XC_trek", "XC_ul_angle", "XC_umbrella",
            "XC_ur_angle", "XC_watch", "XC_xterm"};

END XCursors.
