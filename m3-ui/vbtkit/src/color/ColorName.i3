(* Copyright (C) 1992, Digital Equipment Corporation              *)
(* All rights reserved.                                           *)
(* See the file COPYRIGHT for a full description.                 *)
(* Created by stolfi on Wed Apr 19 01:33:17 1989                  *)
(* Last modified on Mon Jul 12 21:06:12 PDT 1993 by mhb       *)
(*      modified on Fri May 14 16:45:56 PDT 1993 by meehan    *)
(*      modified on Wed Jun  3 18:06:16 PDT 1992 by stolfi        *)
(*      modified on Tue Feb 11 21:39:49 PST 1992 by muller        *)

(* The "ColorName" interface provides a standard mapping between color
   names and linear RGB triples. The implementation recognizes
   the following names, based on those found in 
   "/usr/lib/X11/rgb.txt": 

    \begin{center}
    \begin{tt}
    \begin{tabular}{llll}

    AliceBlue         & ForestGreen    & MintCream         & SandyBrown \\
    AntiqueWhite \dag & Gainsboro      & MistyRose \dag    & SeaGreen \dag \\
    Aquamarine \dag   & GhostWhite     & Moccasin          & Seashell \dag \\
    Azure \dag        & Gold \dag      & NavajoWhite \dag  & Sienna \dag \\
    Beige             & Goldenrod \dag & Navy              & SkyBlue \dag \\
    Bisque            & GoldenrodYellow& NavyBlue          & SlateBlue \dag \\
    Black             & Gray \ddag     & OldLace           & SlateGray \dag \\
    BlanchedAlmond    & Green \dag     & OliveDrab \dag    & SlateGrey \\
    Blue \dag         & GreenYellow    & OliveGreen \dag   & Snow \dag \\
    BlueViolet        & Grey \ddag     & Orange \dag       & SpringGreen\dag \\
    Brown \dag        & Honeydew \dag  & OrangeRed \dag    & SteelBlue \dag \\
    Burlywood \dag    & HotPink \dag   & Orchid \dag       & Tan \dag \\
    CadetBlue \dag    & IndianRed \dag & PapayaWhip        & Thistle \dag \\
    Chartreuse \dag   & Ivory \dag     & PeachPuff \dag    & Tomato \dag \\
    Chocolate \dag    & Khaki \dag     & Peru              & Turquoise \dag \\
    Coral \dag        & Lavender       & Pink \dag         & Violet \\
    CornflowerBlue    & LavenderBlush \dag & Plum \dag     & VioletRed \dag \\
    Cornsilk \dag     & LawnGreen          & Powderblue    & Wheat \dag \\
    Cyan \dag         & LemonChiffon \dag  & Purple \dag   & White \\
    DeepPink \dag     & LimeGreen          & Red \dag      & WhiteSmoke \\
    DeepSkyBlue \dag  & Linen              & RosyBrown\dag & Yellow \dag \\
    DodgerBlue \dag   & Magenta \dag       & Royalblue\dag & YellowGreen \\
    Firebrick \dag    & Maroon \dag        & SaddleBrown \\
    FloralWhite       & MidnightBlue       & Salmon \dag \\

    \end{tabular}
    \end{tt}
    \end{center}

    The dagger (\dag) indicates that the implementation recognizes
    a name along with the suffixes "1"--"4"; e.g., "Red", "Red1", "Red2",
    "Red3", and "Red4".
    
   The double dagger (\ddag) indicates that the 
   implementation also recognizes the names with the 
   suffixes "0" through "100". That is, "Gray0", "Gray1", \dots,
   "Gray100", as well as "Grey0", "Grey1", \dots, "Grey100".
   
   In addition, the name of a color $C$ from this list
   can be prefixed by one or more of the following modifiers:


\begin{center}
\begin{tabular}{l|l}
% 
  \it Term & \it Meaning\\ \hline

  \strut\begin{tabular}[c]{l} 
  "Light" \\ 
  "Pale" 
  \end{tabular} 
    &  1/3 of the way from $C$ to white \\ \hline

  \strut\begin{tabular}[c]{l} 
  "Dark" \\ 
  "Dim" 
  \end{tabular} 
    &  1/3 of the way from $C$ to black \\ \hline

  \strut\begin{tabular}[c]{l} 
  "Drab" \\ 
  "Weak" \\ 
  "Dull" 
  \end{tabular} 
    & $\vcenter{\hbox{ 1/3 of the way from $C$ to the gray\strut}
               \hbox{with the same brightness as $C$\strut}}$ \\ \hline

  \strut\begin{tabular}[c]{l} 
  "Vivid" \\ 
  "Strong" \\ 
  "Bright" 
  \end{tabular} 
    & $\vcenter{\hbox{1/3 of the way from $C$ to the purest color\strut}
               \hbox{with the same hue as $C$\strut}}$ \\ \hline

  \strut\begin{tabular}[c]{l} 
  "Reddish" 
  \end{tabular}  
     &  1/3 of the way from $C$ to red \\ \hline

  \strut\begin{tabular}[c]{l} 
  "Greenish" 
  \end{tabular}  
    &  1/3 of the way from $C$ to green \\ \hline

  \strut\begin{tabular}[c]{l} 
  "Bluish" 
  \end{tabular}  
    &  1/3 of the way from $C$ to blue \\ \hline

  \strut\begin{tabular}[c]{l} 
  "Yellowish" 
  \end{tabular}  
    &  1/3 of the way from $C$ to yellow \\
%
\end{tabular}
\end{center}

   Each of these modifiers can be modified in turn by the following
   prefixes, which replace ``1/3 of the way'' by the indicated fraction:

   \begin{center}
   \begin{tabular}{l|l|l}
      \it Term             & \it Degree        & \it\% (approx.) \\ \hline
      "VeryVerySlightly"   & 1/16 of the way   &  6\% \\
      "VerySlightly"       & 1/8 of the way    & 13\% \\
      "Slightly"           & 1/4 of the way    & 25\% \\
      "Somewhat"           & 3/8 of the way    & 38\% \\ 
      "Rather"             & 1/2 of the way    & 50\% \\ 
      "Quite"              & 5/8 of the way    & 63\% \\
      "Very"               & 3/4 of the way    & 75\% \\
      "VeryVery"           & 7/8 of the way    & 88\% \\
      "VeryVeryVery"       & 15/16 of the way  & 94\% \\
   \end{tabular}
   \end{center}

   \noindent The modifier "Medium" is also recognized as a
   shorthand for "SlightlyDark". (But you cannot use "VeryMedium".)

*)

INTERFACE ColorName;

IMPORT Color, TextList;

EXCEPTION NotFound;

PROCEDURE ToRGB (name: TEXT): Color.T RAISES {NotFound};
(* Give the "RGB.T" value described by "name", ignoring case and
   whitespace.  A cache of unnormalized names is maintained, so
   this procedure should be pretty fast for repeated lookups of
   the same name. *)

PROCEDURE NameList (): TextList.T;
(* Return a list of all the ``basic'' (unmodified) color names known
   to this module, as lower-case "TEXT"s, in alphabetical order. *)

END ColorName.

