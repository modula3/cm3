#include <windows.h>
#include <windowsx.h>

#ifdef __cplusplus
extern "C" {
#endif

void
__cdecl
WinGDI__MAKEPOINTS(LPARAM lParam, POINTS* points) // s is for 16bit signed short, not plural
{
      // #define MAKEPOINTS(l) (*((POINTS*)&(l)))
      *points = MAKEPOINTS(lParam);
}

LPARAM
__cdecl
WinGDI__PointsToLParam(const POINTS* points) // s is for 16bit signed short, not plural
// This is not in windows.h but follows from previous.
// LPARAM is more aligned than points so use union,
// instead of *(LPARAM*)points. LPARAM is size_t. POINTS is shorts.
{
    union
    {
        POINTS points;
        LPARAM lParam;
    } u = {*points};
    return u.lParam;
}

int
__cdecl
WinGDI__GET_X_LPARAM(LPARAM lParam)
{
    return GET_X_LPARAM(lParam);
}

int
__cdecl
WinGDI__GET_Y_LPARAM(LPARAM lParam)
{
    return GET_Y_LPARAM(lParam);
}

#ifdef __cplusplus
}
#endif
