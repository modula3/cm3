(* 
 * This library is free software; you can redistribute it and/or          
 * modify it under the terms of the GNU Library General Public            
 * License as published by the Free Software Foundation.                  
 * This library is distributed in the hope that it will be useful,        
 * but WITHOUT ANY WARRANTY; without even the implied warranty of         
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      
 * Library General Public License for more details.                       
 * If you do not have a copy of the GNU Library General Public            
 * License, write to The Free Software Foundation, Inc.,                  
 * 675 Mass Ave, Cambridge, MA 02139, USA.                                
 *                                                                        
 * For more information on this program, contact Blair MacIntyre          
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 500 W 120th St, Room 450, New York, NY, 10027.                         
 *                                                                        
 * Copyright (C) Blair MacIntyre 1995, Columbia University 1995           
 * 
 * Author          : Blair MacIntyre
 * Created On      : Tue Aug  8 16:28:33 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Dec  5 13:14:24 1995
 * Update Count    : 8
 * 
 * SCCS Status     : @(#)ObSharedObj.i3	1.2	30 Jan 1996
 * 
 * HISTORY
 *)

INTERFACE ObSharedObj;

IMPORT ObLib, SynLocation, ObValue, SharedObj, ObLoader, ObEmbProxiedObj;

PROCEDURE SetupPackage ();
PROCEDURE SetupModule (loader : ObLoader.T);

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : SharedObj.T 
    RAISES {ObValue.Error, ObValue.Exception};

(* The two SharedObj exceptions. *)
VAR errorException, fatalException: ObValue.Val;

TYPE T <: ObEmbProxiedObj.T;

END ObSharedObj. 
