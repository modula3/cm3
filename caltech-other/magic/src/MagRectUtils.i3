INTERFACE MagRectUtils;
IMPORT MagRect;
IMPORT Rect;
TYPE
  T = MagRect.T;

PROCEDURE FromRect(r: Rect.T): T;
PROCEDURE ToRect(mr: T): Rect.T;

END MagRectUtils.
