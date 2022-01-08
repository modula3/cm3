/* Copyright (C) 1994, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */
/*                                                            */
/* Last modified on Wed Aug 17 14:25:29 PDT 1994 by kalsow    */
/*                                                            */
/* Originally submitted on Fri, 22 Jul 1994 16:42:53 GMT      */
/*   by jredford@lehman.com (John Redford)                    */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifndef _WIN32

M3EXTERNC_BEGIN

struct m3_group_t
/* This MUST match Ugrp.i3 */
{
    char** mem;
    char* name;
    m3_gid_t gid;
};

static m3_group_t* native_to_m3group(const struct group* native, m3_group_t* m3)
{
    if (native == NULL)
    {
        m3 = NULL;
    }
    else
    {
        m3->name = native->gr_name;
        m3->gid = native->gr_gid;
        m3->mem = native->gr_mem;
    }
    Scheduler__EnableSwitching();
    return m3;
}

m3_group_t* __cdecl
Ugrp__getgrent(m3_group_t* m3group)
{
    Scheduler__DisableSwitching();
    return native_to_m3group(getgrent(), m3group);
}

m3_group_t* __cdecl
Ugrp__getgrgid(m3_group_t* m3group, m3_gid_t gid)
{
    Scheduler__DisableSwitching();
    return native_to_m3group(getgrgid(gid), m3group);
}

m3_group_t* __cdecl
Ugrp__getgrnam(m3_group_t* m3group, const char* name)
{
    Scheduler__DisableSwitching();
    return native_to_m3group(getgrnam(name), m3group);
}

void __cdecl
Ugrp__setgrent(void)
{
    setgrent();
}

void __cdecl
Ugrp__endgrent(void)
{
    endgrent();
}

M3EXTERNC_END

#endif
