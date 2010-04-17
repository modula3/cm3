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
int MachineID__CanGetWithNetbios(unsigned char *id)
{
    NCB ncb = { 0 };
    LANA_ENUM lanaEnum = { 0 };
    ADAPTER_STATUS adaptorStatus = { 0 };

    ZeroMemory(id, 6);
    ZeroMemory(&ncb, sizeof(ncb));
    ZeroMemory(&adaptorStatus, sizeof(adaptorStatus));
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
            ncb.ncb_buffer = (PUCHAR)&adaptorStatus;
            ncb.ncb_length = sizeof(adaptorStatus);
            Netbios(&ncb);
            if (ncb.ncb_retcode == 0)
            {
                memcpy(id, &adaptorStatus.adapter_address, 6);
                return TRUE;
            }
        }
    }

    /* failed */
    return FALSE;
}

int MachineID__CanGet(unsigned char *id)
{
    union {
        UUID uuid;
        unsigned char bytes[16];
    } u = { 0 };
    RPC_STATUS status = { 0 };
    typedef RPC_STATUS (RPC_ENTRY * PFN)(UUID*);
    static PFN pfn;
    HMODULE module;

    ZeroMemory(id, 6);

    if (MachineID__CanGetWithNetbios(id))
        return TRUE;

    if (pfn == NULL)
    {
        module = LoadLibrary("rpcrt4.dll");
        if (module == NULL)
            return 0;
        pfn = (PFN)GetProcAddress(module, "UuidCreateSequential");
        if (pfn == NULL)
        {
            /* Do not fall back if UuidCreateSequential is supposed be there.
             */
            if ((GetVersion() & 0xFF) >= 5)
                return FALSE;

            pfn = UuidCreate;
        }
    }

    status = (*pfn)(&u.uuid);
    id[0] = u.bytes[10];
    id[1] = u.bytes[11];
    id[2] = u.bytes[12];
    id[3] = u.bytes[13];
    id[4] = u.bytes[14];
    id[5] = u.bytes[15];
    
    return (status == RPC_S_OK);
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#if 0 /* test code */

#include <stdio.h>

int main()
{
    unsigned char id[6] = { 0 };
    int i = { 0 };
    
    i = MachineID__CanGet((char*)id);
    printf("%d %02x%02x%02x%02x%02x%02x\n", i, id[0], id[1], id[2], id[3], id[4], id[5]);

    i = MachineID__CanGetWithNetbios((char*)id);
    printf("%d %02x%02x%02x%02x%02x%02x\n", i, id[0], id[1], id[2], id[3], id[4], id[5]);

    return 0;
}

#endif
