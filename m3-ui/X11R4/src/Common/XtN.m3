(* Copyright (C) 1989, 1990 Digital Equipment Corporation	*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)

(* File: XtN.i3							*)
(* Last modified on Fri Apr 13 14:17:04 1990 by jerome		*)
(*      modified on Sat Feb 24 02:19:18 1990 by muller		*)


UNSAFE MODULE XtN;

(*==============================================================*)
(*	The X11 R4 Interface for Modula 3			*)
(*								*)
(*	Resource names corresponding to:			*)
(*			X11 R4 Intrinsic			*)
(*			X11 R4 Athena Widget Set		*)
(*==============================================================*)


FROM M3toC IMPORT TtoS;

PROCEDURE ForceToLoadAnImplementation () =
BEGIN
END ForceToLoadAnImplementation;

BEGIN

  accelerators                   := TtoS("accelerators");
  allowHoriz                     := TtoS("allowHoriz");
  allowResize                    := TtoS("allowResize");
  allowShellResize               := TtoS("allowShellResize");
  allowVert                      := TtoS("allowVert");
  analog                         := TtoS("analog");
  ancestorSensitive              := TtoS("ancestorSensitive");
  argc                           := TtoS("argc");
  argv                           := TtoS("argv");
  autoFill                       := TtoS("autoFill");
  background                     := TtoS("background");
  backgroundPixmap               := TtoS("backgroundPixmap");
  backingStore                   := TtoS("backingStore");
  baseHeight                     := TtoS("baseHeight");
  baseWidth                      := TtoS("baseWidth");
  betweenCursor                  := TtoS("betweenCursor");
  bitmap                         := TtoS("bitmap");
  border                         := TtoS("borderColor");
  borderColor                    := TtoS("borderColor");
  borderPixmap                   := TtoS("borderPixmap");
  borderWidth                    := TtoS("borderWidth");
  bottom                         := TtoS("bottom");
  bottomMargin                   := TtoS("bottomMargin");
  callback                       := TtoS("callback");
  checkCommand                   := TtoS("checkCommand");
  children                       := TtoS("children");
  chime                          := TtoS("chime");
  colormap                       := TtoS("colormap");
  columnSpacing                  := TtoS("columnSpacing");
  columnWidth                    := TtoS("columnWidth");
  cornerRoundPercent             := TtoS("cornerRoundPercent");
  createPopupChildProc           := TtoS("createPopupChildProc");
  cursor                         := TtoS("cursor");
  dataCompression                := TtoS("dataCompression");
  defaultColumns                 := TtoS("defaultColumns");
  defaultDistance                := TtoS("defaultDistance");
  depth                          := TtoS("depth");
  destroyCallback                := TtoS("destroyCallback");
  dialogHOffset                  := TtoS("dialogHOffset");
  dialogVOffset                  := TtoS("dialogVOffset");
  displayCaret                   := TtoS("displayCaret");
  displayNonprinting             := TtoS("displayNonprinting");
  displayPosition                := TtoS("displayPosition");
  echo                           := TtoS("echo");
  editType                       := TtoS("editType");
  emptyPixmap                    := TtoS("emptyPixmap");
  emptyPixmapMask                := TtoS("emptyPixmapMask");
  file                           := TtoS("file");
  flip                           := TtoS("flip");
  font                           := TtoS("font");
  forceBars                      := TtoS("forceBars");
  forceColumns                   := TtoS("forceColumns");
  foreground                     := TtoS("foreground");
  fromHoriz                      := TtoS("fromHoriz");
  fromVert                       := TtoS("fromVert");
  fullPixmap                     := TtoS("fullPixmap");
  fullPixmapMask                 := TtoS("fullPixmapMask");
  function                       := TtoS("function");
  geometry                       := TtoS("geometry");
  getValue                       := TtoS("getValue");
  gripCursor                     := TtoS("gripCursor");
  gripIndent                     := TtoS("gripIndent");
  gripTranslations               := TtoS("gripTranslations");
  hSpace                         := TtoS("hSpace");
  hand                           := TtoS("hands");
  height                         := TtoS("height");
  heightInc                      := TtoS("heightInc");
  highlight                      := TtoS("highlight");
  highlightThickness             := TtoS("highlightThickness");
  horizDistance                  := TtoS("horizDistance");
  horizontalBetweenCursor        := TtoS("horizontalBetweenCursor");
  horizontalGripCursor           := TtoS("horizontalGripCursor");
  icon                           := TtoS("icon");
  iconMask                       := TtoS("iconMask");
  iconName                       := TtoS("iconName");
  iconNameEncoding               := TtoS("iconNameEncoding");
  iconPixmap                     := TtoS("iconPixmap");
  iconWindow                     := TtoS("iconWindow");
  iconX                          := TtoS("iconX");
  iconY                          := TtoS("iconY");
  iconic                         := TtoS("iconic");
  index                          := TtoS("index");
  initialResourcesPersistent     := TtoS("initialResourcesPersistent");
  initialState                   := TtoS("initialState");
  innerHeight                    := TtoS("innerHeight");
  innerWidth                     := TtoS("innerWidth");
  innerWindow                    := TtoS("innerWindow");
  input                          := TtoS("input");
  insensitiveBorder              := TtoS("insensitiveBorder");
  insertPosition                 := TtoS("insertPosition");
  internalBorderColor            := TtoS("internalBorderColor");
  internalBorderWidth            := TtoS("internalBorderWidth");
  internalHeight                 := TtoS("internalHeight");
  internalWidth                  := TtoS("internalWidth");
  jumpProc                       := TtoS("jumpProc");
  jumpScroll                     := TtoS("jumpScroll");
  justify                        := TtoS("justify");
  knobHeight                     := TtoS("knobHeight");
  knobIndent                     := TtoS("knobIndent");
  knobPixel                      := TtoS("knobPixel");
  knobWidth                      := TtoS("knobWidth");
  label                          := TtoS("label");
  labelClass                     := TtoS("labelClass");
  left                           := TtoS("left");
  leftBitmap                     := TtoS("leftBitmap");
  leftCursor                     := TtoS("leftCursor");
  leftMargin                     := TtoS("leftMargin");
  length                         := TtoS("length");
  lineWidth                      := TtoS("lineWidth");
  list                           := TtoS("list");
  longest                        := TtoS("longest");
  lowerCursor                    := TtoS("lowerCursor");
  lowerRight                     := TtoS("lowerRight");
  mappedWhenManaged              := TtoS("mappedWhenManaged");
  max                            := TtoS("max");
  maxAspectX                     := TtoS("maxAspectX");
  maxAspectY                     := TtoS("maxAspectY");
  maxHeight                      := TtoS("maxHeight");
  maxWidth                       := TtoS("maxWidth");
  menuEntry                      := TtoS("menuEntry");
  menuName                       := TtoS("menuName");
  menuOnScreen                   := TtoS("menuOnScreen");
  min                            := TtoS("min");
  minAspectX                     := TtoS("minAspectX");
  minAspectY                     := TtoS("minAspectY");
  minHeight                      := TtoS("minHeight");
  minScale                       := TtoS("minScale");
  minWidth                       := TtoS("minWidth");
  minimumThumb                   := TtoS("minimumThumb");
  name                           := TtoS("name");
  notify                         := TtoS("notify");
  numChildren                    := TtoS("numChildren");
  numberStrings                  := TtoS("numberStrings");
  onceOnly                       := TtoS("onceOnly");
  orientation                    := TtoS("orientation");
  overrideRedirect               := TtoS("overrideRedirect");
  padding                        := TtoS("padding");
  parameter                      := TtoS("parameter");
  pasteBuffer                    := TtoS("pasteBuffer");
  pieceSize                      := TtoS("pieceSize");
  pixmap                         := TtoS("pixmap");
  popdownCallback                := TtoS("popdownCallback");
  popupCallback                  := TtoS("popupCallback");
  popupOnEntry                   := TtoS("popupOnEntry");
  position                       := TtoS("position");
  preferredPaneSize              := TtoS("preferredPaneSize");
  radioData                      := TtoS("radioData");
  radioGroup                     := TtoS("radioGroup");
  refigureMode                   := TtoS("refigureMode");
  resizable                      := TtoS("resizable");
  resize                         := TtoS("resize");
  resizeToPreferred              := TtoS("resizeToPreferred");
  reverseVideo                   := TtoS("reverseVideo");
  right                          := TtoS("right");
  rightBitmap                    := TtoS("rightBitmap");
  rightCursor                    := TtoS("rightCursor");
  rightMargin                    := TtoS("rightMargin");
  rowHeight                      := TtoS("rowHeight");
  rowSpacing                     := TtoS("rowSpacing");
  saveUnder                      := TtoS("saveUnder");
  scale                          := TtoS("scale");
  screen                         := TtoS("screen");
  scrollDCursor                  := TtoS("scrollDCursor");
  scrollHCursor                  := TtoS("scrollHCursor");
  scrollHorizontal               := TtoS("scrollHorizontal");
  scrollLCursor                  := TtoS("scrollLCursor");
  scrollProc                     := TtoS("scrollProc");
  scrollRCursor                  := TtoS("scrollRCursor");
  scrollUCursor                  := TtoS("scrollUCursor");
  scrollVCursor                  := TtoS("scrollVCursor");
  scrollVertical                 := TtoS("scrollVertical");
  selectTypes                    := TtoS("selectTypes");
  selection                      := TtoS("selection");
  selectionArray                 := TtoS("selectionArray");
  sensitive                      := TtoS("sensitive");
  shapeStyle                     := TtoS("shapeStyle");
  shapeWindow                    := TtoS("shapeWindow");
  showGrip                       := TtoS("showGrip");
  shown                          := TtoS("shown");
  skipAdjust                     := TtoS("skipAdjust");
  space                          := TtoS("space");
  state                          := TtoS("state");
  stipple                        := TtoS("stipple");
  string                         := TtoS("string");
  templateResource               := TtoS("templateResource");
  textOptions                    := TtoS("textOptions");
  textSink                       := TtoS("textSink");
  textSource                     := TtoS("textSource");
  thickness                      := TtoS("thickness");
  thumb                          := TtoS("thumb");
  thumbProc                      := TtoS("thumbProc");
  title                          := TtoS("title");
  titleEncoding                  := TtoS("titleEncoding");
  top                            := TtoS("top");
  topMargin                      := TtoS("topMargin");
  topOfThumb                     := TtoS("topOfThumb");
  transient                      := TtoS("transient");
  transientFor                   := TtoS("transientFor");
  translations                   := TtoS("translations");
  type                           := TtoS("type");
  unrealizeCallback              := TtoS("unrealizeCallback");
  update                         := TtoS("update");
  upperCursor                    := TtoS("upperCursor");
  useBottom                      := TtoS("useBottom");
  useRight                       := TtoS("useRight");
  useStringInPlace               := TtoS("useStringInPlace");
  vSpace                         := TtoS("vSpace");
  value                          := TtoS("value");
  vertDistance                   := TtoS("vertDistance");
  vertSpace                      := TtoS("vertSpace");
  verticalBetweenCursor          := TtoS("verticalBetweenCursor");
  verticalGripCursor             := TtoS("verticalGripCursor");
  verticalList                   := TtoS("verticalList");
  visual                         := TtoS("visual");
  vmunix                         := TtoS("vmunix");
  volume                         := TtoS("volume");
  waitForWm                      := TtoS("waitforwm");
  width                          := TtoS("width");
  widthInc                       := TtoS("widthInc");
  winGravity                     := TtoS("winGravity");
  window                         := TtoS("window");
  windowGroup                    := TtoS("windowGroup");
  wmTimeout                      := TtoS("wmTimeout");
  wrap                           := TtoS("wrap");
  x                              := TtoS("x");
  y                              := TtoS("y");

END XtN.
