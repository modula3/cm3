/* Copyright (C) 1995, Digital Equipment Corporation              */
/* All rights reserved.                                           */
/* See the file COPYRIGHT for a full description.                 */
/*                                                                */
/* Last modified on Tue May  2 08:05:30 PDT 1995 by kalsow        */
/*                                                                */
/* contributed by Derek Beatty <beatty@beatty.slip.netcom.com>,   */
/*   April 26, 1995                                               */

#include <mach/mach.h>
#include <mach/mach_error.h>

vm_address_t
sbrk(
    vm_size_t uSize
    )
    {
    vm_address_t vatReturn= 0;
    kern_return_t krtOk;
    

    krtOk= vm_allocate( task_self(), &vatReturn, uSize, TRUE);
    if (krtOk != KERN_SUCCESS)
	{
	mach_error("vm_allocate failed", krtOk);
	return 0;
	}
    else
	{
	return vatReturn;
	}
    }
   
/*************  
Date: Wed, 26 Apr 95 19:19:08 -0400
From: Derek Lee Beatty <beatty@beatty.slip.netcom.com>
To: kalsow@pa.dec.com
Subject: Re: building 3.5.2 on a NEXT 
Reply-To: beatty@netcom.com

The following did the trick; the compiler seems to be running fine  
now.  Thanks for your help.
There are a couple of other issues I have to resolve, but things are  
a lot smoother now.

 -- Derek

**************/

