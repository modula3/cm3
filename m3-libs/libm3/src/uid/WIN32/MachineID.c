/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Mon Sep 20 11:46:17 PDT 1993 by kalsow     */
/*      modified on Thu Jul 15 16:23:08 PDT 1993 by swart      */

#include <windows.h>
#include <rpc.h>
#include <string.h>
#include <memory.h>
#include <nb30.h>

#ifdef __cplusplus
extern "C" {
#endif

static
BOOL
GetMacAddressFromNetbios(unsigned char *id)
{
    NCB ncb = { 0 };
    LANA_ENUM lanaEnum = { 0 };
    struct {
        ADAPTER_STATUS adaptorStatus;
        NAME_BUFFER names[30];
    } adaptorStatusBuffer;

    ZeroMemory(id, 6);
    ZeroMemory(&ncb, sizeof(ncb));
    ZeroMemory(&adaptorStatusBuffer, sizeof(adaptorStatusBuffer));
    ZeroMemory(&lanaEnum, sizeof(lanaEnum));
    ncb.ncb_command = NCBENUM;
    ncb.ncb_buffer = (PUCHAR)&lanaEnum;
    ncb.ncb_length = sizeof(lanaEnum);
    Netbios(&ncb);
    if ((ncb.ncb_retcode == 0) && (lanaEnum.length >= 1))
    {
        ZeroMemory(&ncb, sizeof(ncb));
        ncb.ncb_command = NCBRESET;
        ncb.ncb_lana_num = lanaEnum.lana[0];
        ncb.ncb_lsn = 0;
        ncb.ncb_num = 0;
        ncb.ncb_buffer = NULL;
        ncb.ncb_length = 0;
        Netbios(&ncb);
        if (ncb.ncb_retcode == 0)
        {
            ncb.ncb_command = NCBASTAT;
            ncb.ncb_callname[0] = '*';
            ncb.ncb_callname[1] = 0;
            ncb.ncb_buffer = (PUCHAR)&adaptorStatusBuffer;
            ncb.ncb_length = sizeof(adaptorStatusBuffer);
            Netbios(&ncb);
            if (ncb.ncb_retcode == 0)
            {
                memcpy(id, &adaptorStatusBuffer.adaptorStatus.adapter_address, 6);
                return TRUE;
            }
        }
    }

    /* failed */
    return FALSE;
}

static
BOOL
GetMacAddressFromUuidCreateSequential(unsigned char *id)
{
    union {
        unsigned char bytes[16];
        UUID uuid;
    } u = { 0 };
    RPC_STATUS status = { 0 };
    typedef RPC_STATUS (RPC_ENTRY * PFN)(UUID*);
    static PFN pfn;
    HMODULE module = { 0 };

    ZeroMemory(id, 6);
    ZeroMemory(&u, sizeof(u));

    if (pfn == NULL)
    {
        module = LoadLibrary("rpcrt4.dll");
        if (module == NULL)
            return FALSE;
        pfn = (PFN)GetProcAddress(module, "UuidCreateSequential");
        if (pfn == NULL)
        {
            /* Do not fall back if UuidCreateSequential is supposed be there.
             */
            if ((GetVersion() & 0xFF) >= 5)
                return FALSE;

            pfn = &UuidCreate;
        }
    }

    status = (*pfn)(&u.uuid);
    memcpy(id, &u.bytes[10], 6);    
    return (status == RPC_S_OK);
}

BOOL
__cdecl
MachineIDC__CanGet(unsigned char *id)
{
    ZeroMemory(id, 6);

    return (GetMacAddressFromNetbios(id) || GetMacAddressFromUuidCreateSequential(id));
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#if 1 /* test code */

#include <stdio.h>

int main()
{
    unsigned char id[6] = { 0 };
    int i = { 0 };
    
    i = GetMacAddressFromNetbios(id);
    printf("%d %02x%02x%02x%02x%02x%02x\n", i, id[0], id[1], id[2], id[3], id[4], id[5]);

    i = GetMacAddressFromUuidCreateSequential(id);
    printf("%d %02x%02x%02x%02x%02x%02x\n", i, id[0], id[1], id[2], id[3], id[4], id[5]);

    return 0;
}

#endif
