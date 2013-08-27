/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Mon Sep 20 11:46:17 PDT 1993 by kalsow     */
/*      modified on Thu Jul 15 16:23:08 PDT 1993 by swart      */

#include <string.h>
#include <memory.h>
#include <winsock2.h>
#include <iphlpapi.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/* older cm3 config files do not link with iphlpapi.lib so use LoadLibrary/GetProcAddress */
typedef DWORD (__stdcall * GetIfTable_t)(PMIB_IFTABLE pIfTable, PULONG pdwSize, BOOL bOrder);

int/*BOOL*/
__cdecl
MachineIDC__CanGet(unsigned char* id)
{
    BOOL Success = { 0 };
    DWORD Error = { 0 };
    DWORD Size = { 0 };
    MIB_IFTABLE* Table = { 0 };
    DWORD i = { 0 };
    DWORD NumEntries = { 0 };
    static GetIfTable_t s_getIfTable;
    GetIfTable_t getIfTable = s_getIfTable;

    ZeroMemory(id, 6);
    
    if (getIfTable == NULL)
    {
        HMODULE hmodule = LoadLibraryW(L"iphlpapi.dll");
        if (hmodule == NULL)
            goto Exit;
        getIfTable = (GetIfTable_t)GetProcAddress(hmodule, "GetIfTable");
        if (getIfTable == NULL)
            goto Exit;
        s_getIfTable = getIfTable;
    }

    /* Call until it fits, typically twice. */
    Size = 0;
    while ((Error = getIfTable(Table, &Size, FALSE)) == ERROR_INSUFFICIENT_BUFFER)
    {
        free(Table);
        Table = (MIB_IFTABLE*)malloc(Size);
        if (Table == NULL)
        {
            Error = ERROR_NOT_ENOUGH_MEMORY;
            goto Exit;
        }
    }
    if (Error)
        goto Exit;

    NumEntries = Table->dwNumEntries;
    for (i = 0; (!Success) && (i < NumEntries); ++i)
    {
        MIB_IFROW* Row = &Table->table[i];
        if (Row->dwType != IF_TYPE_ETHERNET_CSMACD || Row->dwPhysAddrLen != 6)
            continue;
        memcpy(id, Row->bPhysAddr, 6);
        Success = TRUE;
    }
Exit:
    free(Table);
    return Success;
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#if 0 /* test code */

#include <stdio.h>

int main()
{
    unsigned char id[6] = { 0 };
    BOOL i = { 0 };
    
    i = MachineIDC__CanGet(id);
    printf("%d %02X-%02X-%02X-%02X-%02X-%02X\n", i, id[0], id[1], id[2], id[3], id[4], id[5]);
    system("getmac");

    return 0;
}

#endif
