INTERFACE WinImageList;


(* 
	Based on commctrl.h version 1.2
	Copyright 1991-1998, Microsoft Corp. All rights reserved.
	Copyright Darko Volaric 2002 darko@peter.com.au
*)

FROM WinBaseTypes IMPORT DWORD;
FROM Ctypes IMPORT int;
FROM WinDef IMPORT HDC;
FROM WinDef IMPORT COLORREF;
FROM WinBaseTypes IMPORT UINT;
FROM WinDef IMPORT HBITMAP;
FROM WinDef IMPORT RECT;
FROM WinDef IMPORT HICON;
FROM WinDef IMPORT BOOL;
FROM WinNT IMPORT LPCSTR;
FROM WinNT IMPORT LPCWSTR;
FROM WinDef IMPORT HWND;
FROM WinDef IMPORT POINT;
FROM WinDef IMPORT HINSTANCE;

TYPE
	LPSTREAM = ADDRESS; (* not sure where the interface for this is *)


CONST
	CLR_NONE        = 16_FFFFFFFF;
	CLR_DEFAULT      = 16_FF000000;


	ILC_MASK        = 16_0001;
	ILC_COLOR       = 16_0000;
	ILC_COLORDDB      = 16_00FE;
	ILC_COLOR4       = 16_0004;
	ILC_COLOR8       = 16_0008;
	ILC_COLOR16      = 16_0010;
	ILC_COLOR24      = 16_0018;
	ILC_COLOR32      = 16_0020;
	ILC_PALETTE      = 16_0800;


	ILD_NORMAL       = 16_0000;
	ILD_TRANSPARENT    = 16_0001;
	ILD_MASK        = 16_0010;
	ILD_IMAGE       = 16_0020;
	ILD_ROP        = 16_0040;
	ILD_BLEND25      = 16_0002;
	ILD_BLEND50      = 16_0004;
	ILD_OVERLAYMASK    = 16_0F00;

	ILD_SELECTED      = ILD_BLEND50;
	ILD_FOCUS       = ILD_BLEND25;
	ILD_BLEND       = ILD_BLEND50;
	CLR_HILIGHT      = CLR_DEFAULT;

	ILCF_MOVE  					=	16_00000000;
	ILCF_SWAP  					=	16_00000001;


TYPE
	HIMAGELIST = ADDRESS;

	LPIMAGELISTDRAWPARAMS = UNTRACED REF IMAGELISTDRAWPARAMS;
	IMAGELISTDRAWPARAMS = RECORD
		cbSize: DWORD;
		himl: HIMAGELIST;
		i: int;
		hdcDst: HDC;
		x: int;
		y: int;
		cx: int;
		cy: int;
		xBitmap: int;
		yBitmap: int;
		rgbBk: COLORREF;
		rgbFg: COLORREF;
		fStyle: UINT;
		dwRop: DWORD;
	END;

	LPIMAGEINFO = UNTRACED REF IMAGEINFO;
	IMAGEINFO = RECORD
		hbmImage: HBITMAP;
		hbmMask: HBITMAP;
		Unused1: int;
		Unused2: int;
		rcImage: RECT;
	END;




<*EXTERNAL ImageList_Create:WINAPI*>
PROCEDURE Create( cx: int; cy: int; flags: UINT; cInitial: int; cGrow: int): HIMAGELIST;

<*EXTERNAL ImageList_Destroy:WINAPI*>
PROCEDURE Destroy( himl: HIMAGELIST): BOOL;

<*EXTERNAL ImageList_GetImageCount:WINAPI*>
PROCEDURE GetImageCount( himl: HIMAGELIST): int;

<*EXTERNAL ImageList_SetImageCount:WINAPI*>
PROCEDURE SetImageCount( himl: HIMAGELIST; uNewCount: UINT): BOOL;

<*EXTERNAL ImageList_Add:WINAPI*>
PROCEDURE Add( himl: HIMAGELIST; hbmImage: HBITMAP; hbmMask: HBITMAP): int;

<*EXTERNAL ImageList_ReplaceIcon:WINAPI*>
PROCEDURE ReplaceIcon( himl: HIMAGELIST; i: int; hicon: HICON): int;

<*EXTERNAL ImageList_SetBkColor:WINAPI*>
PROCEDURE SetBkColor( himl: HIMAGELIST; clrBk: COLORREF): COLORREF;

<*EXTERNAL ImageList_GetBkColor:WINAPI*>
PROCEDURE GetBkColor( himl: HIMAGELIST): COLORREF;

<*EXTERNAL ImageList_SetOverlayImage:WINAPI*>
PROCEDURE SetOverlayImage( himl: HIMAGELIST; iImage: int; iOverlay: int): BOOL;

<*EXTERNAL ImageList_Draw:WINAPI*>
PROCEDURE Draw( himl: HIMAGELIST; i: int; hdcDst: HDC; x: int; y: int; fStyle: UINT): BOOL;

<*EXTERNAL ImageList_Replace:WINAPI*>
PROCEDURE Replace( himl: HIMAGELIST; i: int; hbmImage: HBITMAP; hbmMask: HBITMAP): BOOL;

<*EXTERNAL ImageList_AddMasked:WINAPI*>
PROCEDURE AddMasked( himl: HIMAGELIST; hbmImage: HBITMAP; crMask: COLORREF): int;

<*EXTERNAL ImageList_DrawEx:WINAPI*>
PROCEDURE DrawEx( himl: HIMAGELIST; i: int; hdcDst: HDC; x: int; y: int; dx: int; dy: int; rgbBk: COLORREF; rgbFg: COLORREF; fStyle: UINT): BOOL;

<*EXTERNAL ImageList_DrawIndirect:WINAPI*>
PROCEDURE DrawIndirect( pimldp: LPIMAGELISTDRAWPARAMS): BOOL;

<*EXTERNAL ImageList_Remove:WINAPI*>
PROCEDURE Remove( himl: HIMAGELIST; i: int): BOOL;

<*EXTERNAL ImageList_GetIcon:WINAPI*>
PROCEDURE GetIcon( himl: HIMAGELIST; i: int; flags: UINT): HICON;

<*EXTERNAL ImageList_LoadImageA:WINAPI*>
PROCEDURE LoadImage( hi: HINSTANCE; lpbmp: LPCSTR; cx: int; cGrow: int; crMask: COLORREF; uType: UINT; uFlags: UINT): HIMAGELIST;

<*EXTERNAL ImageList_LoadImageW:WINAPI*>
PROCEDURE LoadImageW( hi: HINSTANCE; lpbmp: LPCWSTR; cx: int; cGrow: int; crMask: COLORREF; uType: UINT; uFlags: UINT): HIMAGELIST;

<*EXTERNAL ImageList_Copy:WINAPI*>
PROCEDURE Copy( himlDst: HIMAGELIST; iDst: int; himlSrc: HIMAGELIST; iSrc: int; uFlags: UINT): BOOL;

<*EXTERNAL ImageList_BeginDrag:WINAPI*>
PROCEDURE BeginDrag( himlTrack: HIMAGELIST; iTrack: int; dxHotspot: int; dyHotspot: int): BOOL;

<*EXTERNAL ImageList_EndDrag:WINAPI*>
PROCEDURE EndDrag();

<*EXTERNAL ImageList_DragEnter:WINAPI*>
PROCEDURE DragEnter( hwndLock: HWND; x: int; y: int): BOOL;

<*EXTERNAL ImageList_DragLeave:WINAPI*>
PROCEDURE DragLeave( hwndLock: HWND): BOOL;

<*EXTERNAL ImageList_DragMove:WINAPI*>
PROCEDURE DragMove( x: int; y: int): BOOL;

<*EXTERNAL ImageList_SetDragCursorImage:WINAPI*>
PROCEDURE SetDragCursorImage( himlDrag: HIMAGELIST; iDrag: int; dxHotspot: int; dyHotspot: int): BOOL;

<*EXTERNAL ImageList_DragShowNolock:WINAPI*>
PROCEDURE DragShowNolock( fShow: BOOL): BOOL;

<*EXTERNAL ImageList_GetDragImage:WINAPI*>
PROCEDURE GetDragImage( VAR ppt: POINT; VAR pptHotspot: POINT): HIMAGELIST;

<*EXTERNAL ImageList_Read:WINAPI*>
PROCEDURE Read( pstm: LPSTREAM): HIMAGELIST;

<*EXTERNAL ImageList_Write:WINAPI*>
PROCEDURE Write( himl: HIMAGELIST; pstm: LPSTREAM): BOOL;

<*EXTERNAL ImageList_GetIconSize:WINAPI*>
PROCEDURE GetIconSize( himl: HIMAGELIST; VAR cx: int; VAR cy: int): BOOL;

<*EXTERNAL ImageList_SetIconSize:WINAPI*>
PROCEDURE SetIconSize( himl: HIMAGELIST; cx: int; cy: int): BOOL;

<*EXTERNAL ImageList_GetImageInfo:WINAPI*>
PROCEDURE GetImageInfo( himl: HIMAGELIST; i: int; VAR pImageInfo: IMAGEINFO): BOOL;

<*EXTERNAL ImageList_Merge:WINAPI*>
PROCEDURE Merge( himl1: HIMAGELIST; i1: int; himl2: HIMAGELIST; i2: int; dx: int; dy: int): HIMAGELIST;

<*EXTERNAL ImageList_Duplicate:WINAPI*>
PROCEDURE Duplicate( himl: HIMAGELIST): HIMAGELIST;

PROCEDURE AddIcon(himl: HIMAGELIST; hicon: HICON): int;
PROCEDURE RemoveAll(himl: HIMAGELIST): BOOL;
PROCEDURE ExtractIcon(<*UNUSED*>hi: HINSTANCE; himl: HIMAGELIST; i: int): HICON;
PROCEDURE LoadBitmap(hi: HINSTANCE; lpbmp: LPCSTR; cx: int; cGrow: int; crMask: COLORREF): HIMAGELIST;


END WinImageList.
