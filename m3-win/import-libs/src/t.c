

__declspec(dllimport) void __stdcall FindClosePrinterChangeNotification(void* a);
__declspec(dllimport) void __stdcall WinExec(void* a, void* b);
__declspec(dllimport) void __stdcall wvsprintfW(void* a, void* b, void* c);

__declspec(dllexport) void __stdcall Entry(void* a, void* b, void *c)
{
	wvsprintfW(0, "%p", FindClosePrinterChangeNotification);
	wvsprintfW(0, "%p", WinExec);
}
