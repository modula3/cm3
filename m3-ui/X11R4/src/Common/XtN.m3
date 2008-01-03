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


FROM M3toC IMPORT FlatTtoS;

PROCEDURE ForceToLoadAnImplementation () =
BEGIN
END ForceToLoadAnImplementation;

BEGIN

  accelerators                   := FlatTtoS("accelerators");
  allowHoriz                     := FlatTtoS("allowHoriz");
  allowResize                    := FlatTtoS("allowResize");
  allowShellResize               := FlatTtoS("allowShellResize");
  allowVert                      := FlatTtoS("allowVert");
  analog                         := FlatTtoS("analog");
  ancestorSensitive              := FlatTtoS("ancestorSensitive");
  argc                           := FlatTtoS("argc");
  argv                           := FlatTtoS("argv");
  autoFill                       := FlatTtoS("autoFill");
  background                     := FlatTtoS("background");
  backgroundPixmap               := FlatTtoS("backgroundPixmap");
  backingStore                   := FlatTtoS("backingStore");
  baseHeight                     := FlatTtoS("baseHeight");
  baseWidth                      := FlatTtoS("baseWidth");
  betweenCursor                  := FlatTtoS("betweenCursor");
  bitmap                         := FlatTtoS("bitmap");
  border                         := FlatTtoS("borderColor");
  borderColor                    := FlatTtoS("borderColor");
  borderPixmap                   := FlatTtoS("borderPixmap");
  borderWidth                    := FlatTtoS("borderWidth");
  bottom                         := FlatTtoS("bottom");
  bottomMargin                   := FlatTtoS("bottomMargin");
  callback                       := FlatTtoS("callback");
  checkCommand                   := FlatTtoS("checkCommand");
  children                       := FlatTtoS("children");
  chime                          := FlatTtoS("chime");
  colormap                       := FlatTtoS("colormap");
  columnSpacing                  := FlatTtoS("columnSpacing");
  columnWidth                    := FlatTtoS("columnWidth");
  cornerRoundPercent             := FlatTtoS("cornerRoundPercent");
  createPopupChildProc           := FlatTtoS("createPopupChildProc");
  cursor                         := FlatTtoS("cursor");
  dataCompression                := FlatTtoS("dataCompression");
  defaultColumns                 := FlatTtoS("defaultColumns");
  defaultDistance                := FlatTtoS("defaultDistance");
  depth                          := FlatTtoS("depth");
  destroyCallback                := FlatTtoS("destroyCallback");
  dialogHOffset                  := FlatTtoS("dialogHOffset");
  dialogVOffset                  := FlatTtoS("dialogVOffset");
  displayCaret                   := FlatTtoS("displayCaret");
  displayNonprinting             := FlatTtoS("displayNonprinting");
  displayPosition                := FlatTtoS("displayPosition");
  echo                           := FlatTtoS("echo");
  editType                       := FlatTtoS("editType");
  emptyPixmap                    := FlatTtoS("emptyPixmap");
  emptyPixmapMask                := FlatTtoS("emptyPixmapMask");
  file                           := FlatTtoS("file");
  flip                           := FlatTtoS("flip");
  font                           := FlatTtoS("font");
  forceBars                      := FlatTtoS("forceBars");
  forceColumns                   := FlatTtoS("forceColumns");
  foreground                     := FlatTtoS("foreground");
  fromHoriz                      := FlatTtoS("fromHoriz");
  fromVert                       := FlatTtoS("fromVert");
  fullPixmap                     := FlatTtoS("fullPixmap");
  fullPixmapMask                 := FlatTtoS("fullPixmapMask");
  function                       := FlatTtoS("function");
  geometry                       := FlatTtoS("geometry");
  getValue                       := FlatTtoS("getValue");
  gripCursor                     := FlatTtoS("gripCursor");
  gripIndent                     := FlatTtoS("gripIndent");
  gripTranslations               := FlatTtoS("gripTranslations");
  hSpace                         := FlatTtoS("hSpace");
  hand                           := FlatTtoS("hands");
  height                         := FlatTtoS("height");
  heightInc                      := FlatTtoS("heightInc");
  highlight                      := FlatTtoS("highlight");
  highlightThickness             := FlatTtoS("highlightThickness");
  horizDistance                  := FlatTtoS("horizDistance");
  horizontalBetweenCursor        := FlatTtoS("horizontalBetweenCursor");
  horizontalGripCursor           := FlatTtoS("horizontalGripCursor");
  icon                           := FlatTtoS("icon");
  iconMask                       := FlatTtoS("iconMask");
  iconName                       := FlatTtoS("iconName");
  iconNameEncoding               := FlatTtoS("iconNameEncoding");
  iconPixmap                     := FlatTtoS("iconPixmap");
  iconWindow                     := FlatTtoS("iconWindow");
  iconX                          := FlatTtoS("iconX");
  iconY                          := FlatTtoS("iconY");
  iconic                         := FlatTtoS("iconic");
  index                          := FlatTtoS("index");
  initialResourcesPersistent     := FlatTtoS("initialResourcesPersistent");
  initialState                   := FlatTtoS("initialState");
  innerHeight                    := FlatTtoS("innerHeight");
  innerWidth                     := FlatTtoS("innerWidth");
  innerWindow                    := FlatTtoS("innerWindow");
  input                          := FlatTtoS("input");
  insensitiveBorder              := FlatTtoS("insensitiveBorder");
  insertPosition                 := FlatTtoS("insertPosition");
  internalBorderColor            := FlatTtoS("internalBorderColor");
  internalBorderWidth            := FlatTtoS("internalBorderWidth");
  internalHeight                 := FlatTtoS("internalHeight");
  internalWidth                  := FlatTtoS("internalWidth");
  jumpProc                       := FlatTtoS("jumpProc");
  jumpScroll                     := FlatTtoS("jumpScroll");
  justify                        := FlatTtoS("justify");
  knobHeight                     := FlatTtoS("knobHeight");
  knobIndent                     := FlatTtoS("knobIndent");
  knobPixel                      := FlatTtoS("knobPixel");
  knobWidth                      := FlatTtoS("knobWidth");
  label                          := FlatTtoS("label");
  labelClass                     := FlatTtoS("labelClass");
  left                           := FlatTtoS("left");
  leftBitmap                     := FlatTtoS("leftBitmap");
  leftCursor                     := FlatTtoS("leftCursor");
  leftMargin                     := FlatTtoS("leftMargin");
  length                         := FlatTtoS("length");
  lineWidth                      := FlatTtoS("lineWidth");
  list                           := FlatTtoS("list");
  longest                        := FlatTtoS("longest");
  lowerCursor                    := FlatTtoS("lowerCursor");
  lowerRight                     := FlatTtoS("lowerRight");
  mappedWhenManaged              := FlatTtoS("mappedWhenManaged");
  max                            := FlatTtoS("max");
  maxAspectX                     := FlatTtoS("maxAspectX");
  maxAspectY                     := FlatTtoS("maxAspectY");
  maxHeight                      := FlatTtoS("maxHeight");
  maxWidth                       := FlatTtoS("maxWidth");
  menuEntry                      := FlatTtoS("menuEntry");
  menuName                       := FlatTtoS("menuName");
  menuOnScreen                   := FlatTtoS("menuOnScreen");
  min                            := FlatTtoS("min");
  minAspectX                     := FlatTtoS("minAspectX");
  minAspectY                     := FlatTtoS("minAspectY");
  minHeight                      := FlatTtoS("minHeight");
  minScale                       := FlatTtoS("minScale");
  minWidth                       := FlatTtoS("minWidth");
  minimumThumb                   := FlatTtoS("minimumThumb");
  name                           := FlatTtoS("name");
  notify                         := FlatTtoS("notify");
  numChildren                    := FlatTtoS("numChildren");
  numberStrings                  := FlatTtoS("numberStrings");
  onceOnly                       := FlatTtoS("onceOnly");
  orientation                    := FlatTtoS("orientation");
  overrideRedirect               := FlatTtoS("overrideRedirect");
  padding                        := FlatTtoS("padding");
  parameter                      := FlatTtoS("parameter");
  pasteBuffer                    := FlatTtoS("pasteBuffer");
  pieceSize                      := FlatTtoS("pieceSize");
  pixmap                         := FlatTtoS("pixmap");
  popdownCallback                := FlatTtoS("popdownCallback");
  popupCallback                  := FlatTtoS("popupCallback");
  popupOnEntry                   := FlatTtoS("popupOnEntry");
  position                       := FlatTtoS("position");
  preferredPaneSize              := FlatTtoS("preferredPaneSize");
  radioData                      := FlatTtoS("radioData");
  radioGroup                     := FlatTtoS("radioGroup");
  refigureMode                   := FlatTtoS("refigureMode");
  resizable                      := FlatTtoS("resizable");
  resize                         := FlatTtoS("resize");
  resizeToPreferred              := FlatTtoS("resizeToPreferred");
  reverseVideo                   := FlatTtoS("reverseVideo");
  right                          := FlatTtoS("right");
  rightBitmap                    := FlatTtoS("rightBitmap");
  rightCursor                    := FlatTtoS("rightCursor");
  rightMargin                    := FlatTtoS("rightMargin");
  rowHeight                      := FlatTtoS("rowHeight");
  rowSpacing                     := FlatTtoS("rowSpacing");
  saveUnder                      := FlatTtoS("saveUnder");
  scale                          := FlatTtoS("scale");
  screen                         := FlatTtoS("screen");
  scrollDCursor                  := FlatTtoS("scrollDCursor");
  scrollHCursor                  := FlatTtoS("scrollHCursor");
  scrollHorizontal               := FlatTtoS("scrollHorizontal");
  scrollLCursor                  := FlatTtoS("scrollLCursor");
  scrollProc                     := FlatTtoS("scrollProc");
  scrollRCursor                  := FlatTtoS("scrollRCursor");
  scrollUCursor                  := FlatTtoS("scrollUCursor");
  scrollVCursor                  := FlatTtoS("scrollVCursor");
  scrollVertical                 := FlatTtoS("scrollVertical");
  selectTypes                    := FlatTtoS("selectTypes");
  selection                      := FlatTtoS("selection");
  selectionArray                 := FlatTtoS("selectionArray");
  sensitive                      := FlatTtoS("sensitive");
  shapeStyle                     := FlatTtoS("shapeStyle");
  shapeWindow                    := FlatTtoS("shapeWindow");
  showGrip                       := FlatTtoS("showGrip");
  shown                          := FlatTtoS("shown");
  skipAdjust                     := FlatTtoS("skipAdjust");
  space                          := FlatTtoS("space");
  state                          := FlatTtoS("state");
  stipple                        := FlatTtoS("stipple");
  string                         := FlatTtoS("string");
  templateResource               := FlatTtoS("templateResource");
  textOptions                    := FlatTtoS("textOptions");
  textSink                       := FlatTtoS("textSink");
  textSource                     := FlatTtoS("textSource");
  thickness                      := FlatTtoS("thickness");
  thumb                          := FlatTtoS("thumb");
  thumbProc                      := FlatTtoS("thumbProc");
  title                          := FlatTtoS("title");
  titleEncoding                  := FlatTtoS("titleEncoding");
  top                            := FlatTtoS("top");
  topMargin                      := FlatTtoS("topMargin");
  topOfThumb                     := FlatTtoS("topOfThumb");
  transient                      := FlatTtoS("transient");
  transientFor                   := FlatTtoS("transientFor");
  translations                   := FlatTtoS("translations");
  type                           := FlatTtoS("type");
  unrealizeCallback              := FlatTtoS("unrealizeCallback");
  update                         := FlatTtoS("update");
  upperCursor                    := FlatTtoS("upperCursor");
  useBottom                      := FlatTtoS("useBottom");
  useRight                       := FlatTtoS("useRight");
  useStringInPlace               := FlatTtoS("useStringInPlace");
  vSpace                         := FlatTtoS("vSpace");
  value                          := FlatTtoS("value");
  vertDistance                   := FlatTtoS("vertDistance");
  vertSpace                      := FlatTtoS("vertSpace");
  verticalBetweenCursor          := FlatTtoS("verticalBetweenCursor");
  verticalGripCursor             := FlatTtoS("verticalGripCursor");
  verticalList                   := FlatTtoS("verticalList");
  visual                         := FlatTtoS("visual");
  vmunix                         := FlatTtoS("vmunix");
  volume                         := FlatTtoS("volume");
  waitForWm                      := FlatTtoS("waitforwm");
  width                          := FlatTtoS("width");
  widthInc                       := FlatTtoS("widthInc");
  winGravity                     := FlatTtoS("winGravity");
  window                         := FlatTtoS("window");
  windowGroup                    := FlatTtoS("windowGroup");
  wmTimeout                      := FlatTtoS("wmTimeout");
  wrap                           := FlatTtoS("wrap");
  x                              := FlatTtoS("x");
  y                              := FlatTtoS("y");

END XtN.
