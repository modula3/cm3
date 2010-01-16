#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef _MSC_VER
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
EnvFromMain is either char** from main, or char* GetEnvironmentStringsA from WinMain.
Rather than make a coordinated compiler/runtime change, we just ignore
the compiler-provided data and make the runtime always work.
One additional copy of the environment variables is leaked per .exe/.dll.
*/
void* __cdecl RTLinker__GetEnvironmentStrings (void* EnvFromMain)
{
#ifdef _WIN32
    __declspec(dllimport) char** __stdcall GetEnvironmentStringsA(void);
    return GetEnvironmentStringsA ();
#else
    return EnvFromMain;
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
