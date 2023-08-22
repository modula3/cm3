UNSAFE INTERFACE Xpm;

(*==============================================================*)
(* The X11 R4 Interface for Modula 3 *)
(* *)
(* contains: /usr/include/X11/xpm.h *)
(*==============================================================*)

FROM Ctypes IMPORT int, char_star, char_star_star;

FROM X IMPORT DisplayStar, Pixmap, XImageStar, XWindowAttributesStar,
              Drawable ;

<* EXTERNAL XpmReadFileToImage *>
PROCEDURE ReadFileToImage
  (display: DisplayStar;
   fileName : char_star;
   VAR image_return : XImageStar;
   VAR shape_return : XImageStar;
   attributes : XWindowAttributesStar) : int;

<* EXTERNAL XpmReadFileToPixmap *>
PROCEDURE ReadFileToPixmap
  (display: DisplayStar;
   win : Drawable; 
   fileName : char_star;
   VAR pixmap_return : Pixmap;
   VAR shape_return : Pixmap;
   attributes : XWindowAttributesStar) : int;

<* EXTERNAL XpmReadFileToData *>
PROCEDURE ReadFileToData
  ( fileName : char_star;
    VAR data : char_star_star) : int;

<* EXTERNAL XpmCreateImageFromData *>
PROCEDURE CreateImageFromData
  (display: DisplayStar;
   data : char_star_star;
   VAR image_return : XImageStar;
   VAR shape_return : XImageStar;
   attributes : XWindowAttributesStar) : int;

END Xpm.
