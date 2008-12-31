
__declspec(dllimport) void* __stdcall GetEnvironmentStringsA(void);

void* __stdcall GetEnvironmentStrings(void)
{
    return GetEnvironmentStringsA();
}
