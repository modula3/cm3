(*  SGML parser library                                                    *)
(*  Copyright (C) 1997  Michel Dagenais                                    *)
(*                                                                         *)
(*  This library is free software; you can redistribute it and/or          *)
(*  modify it under the terms of the GNU Library General Public            *)
(*  License as published by the Free Software Foundation; either           *)
(*  version 2 of the License, or (at your option) any later version.       *)
(*                                                                         *)
(*  This library is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      *)
(*  Library General Public License for more details.                       *)
(*                                                                         *)
(*  You should have received a copy of the GNU Library General Public      *)
(*  License along with this library; if not, write to the Free             *)
(*  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.     *)
(*                                                                         *)
(*  For more information on this program, contact Michel Dagenais at       *)
(*  dagenais@vlsi.polymtl.ca or Electrical Eng. Dept., Ecole Polytechnique *)
(*  P.O. Box 6079, Station A, Montreal, Quebec, Canada, H3C 3A7.           *)

(* Type T is an SGML.Application which may be used to print back a parsed
   SGML file to the specified writer. It may be inherited from to perform
   some translation on the parsed file content before printing back. *)

INTERFACE SGMLPrint;

IMPORT SGML, Wr, SGMLElementSeq;

TYPE
  T <: Public;

  Public = SGML.Application OBJECT 
      wr: Wr.T;
      stack: SGMLElementSeq.T;
    METHODS
      init(): T;
    END;

(* The sgml file corresponding to the received parsing events is printed
   to the writer contained in wr. A stack of elements entered is maintained
   in stack. This may be used to determine the position in the structure
   tree through the use of the size and get methods of the sequence 
   in stack. *)

END SGMLPrint.

