MODULE ImageInit;
IMPORT WinNT, WinGDI, WinUser;

(*
Historically Image.Raw.xres and Image.Raw.yres were both always 75.0.
Historically that was not too far from what monitors had.
These days (2008) monitors can have much higher dpi
and the values are computed.
*)

VAR
  init: BOOLEAN;
  xres: REAL; (* in pixels per inch *)
  yres: REAL; (* in pixels per inch *)

PROCEDURE Init() =
BEGIN
  WITH
    hwnd = WinUser.GetDesktopWindow(),
    hdc  = WinUser.GetDC(hwnd),
    mm_hor = FLOAT(WinGDI.GetDeviceCaps(hdc, WinGDI.HORZSIZE)),
    mm_ver = FLOAT(WinGDI.GetDeviceCaps(hdc, WinGDI.VERTSIZE)),
    pix_hor = FLOAT(WinUser.GetSystemMetrics(WinUser.SM_CXSCREEN)),
    pix_ver = FLOAT(WinUser.GetSystemMetrics(WinUser.SM_CYSCREEN))
  DO
    yres := (pix_ver / mm_ver * 10.0 * 2.54);
    xres := (pix_hor / mm_hor * 10.0 * 2.54);
  END;
  (* writes to yres and xres must complete before the write to init *)
  WinNT.MemoryBarrier();
  init := TRUE;
END Init;

PROCEDURE GetDefaultXRes(): REAL = (* in pixels per inch *)
BEGIN
    IF NOT init THEN Init() END;
    RETURN xres;
END GetDefaultXRes;

PROCEDURE GetDefaultYRes(): REAL = (* in pixels per inch *)
BEGIN
    IF NOT init THEN Init() END;
    RETURN yres;
END GetDefaultYRes;

BEGIN
END ImageInit.
