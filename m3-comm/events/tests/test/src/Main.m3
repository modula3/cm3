(*                            -*- Mode: Modula-3 -*- 
 * Main.m3 -- 
 * 
 * This library is free software; you can redistribute it and/or          
 * modify it under the terms of the GNU Library General Public            
 * License as published by the Free Software Foundation; either           
 * version 2 of the License, or (at your option) any later version.       
 *                                                                        
 * This library is distributed in the hope that it will be useful,        
 * but WITHOUT ANY WARRANTY; without even the implied warranty of         
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      
 * Library General Public License for more details.                       
 *                                                                        
 * You should have received a copy of the GNU Library General Public      
 * License along with this library; if not, write to the Free             
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.     
 *                                                                        
 * For more information on this program, contact Blair MacIntyre at       
 * bm@cs.columbia.edu or Computer Science Dept., Columbia University,     
 * 500 W 120th St, Room 450, New York, NY, 10027.                         
 *                                                                        
 * Copyright (C) Blair MacIntyre 1995                                     
 * 
 * Author          : Blair MacIntyre
 * Created On      : Sun Jun 18 15:56:44 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sun Jun 18 15:58:50 1995
 * Update Count    : 1
 * Status          : Unknown, Use with caution!
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1.1.1  1996/03/03 19:13:23  bm
 * Imported Sources
 *
 * 
 * HISTORY
 *)

MODULE Main;

IMPORT IO, Fmt;

TYPE T = RECORD x: INTEGER; END;
TYPE U = OBJECT x: INTEGER; END;

BEGIN
  IO.Put("record = " & Fmt.Int(BYTESIZE(T)) & ", Object = " &
    Fmt.Int(BYTESIZE(U)) & "\n");
END Main.
