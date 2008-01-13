

/* winspool.drv */
__declspec(dllimport) void* __stdcall FindClosePrinterChangeNotification(void* a);

/* kernel32.dll */
__declspec(dllimport) void* __stdcall WinExec(void* a, void* b);

/* user32.dll */
__declspec(dllimport) void* __stdcall GetMenu(void* a);

void __stdcall Entry(void* a, void* b, void *c)
{
	GetMenu(FindClosePrinterChangeNotification(0));
	GetMenu(WinExec(0, 0));
}
