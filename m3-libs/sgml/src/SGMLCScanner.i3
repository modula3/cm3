(*  SGML parser library                                                    *)
(*  Copyright (C) 1997 Michel Dagenais                                     *)
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

INTERFACE SGMLCScanner;

(* This interface defines additional fields and methods for the Scanner
   object defined in the generated SGMLC interface. Since SGML does not
   have a context free syntax, the scanner uses a current state to
   guide the token scanning. Furthermore, a state stack is maintained
   to properly handle nested constructs.

   Another annoying "feature" of SGML is the parameter entities which need
   to be replaced before the parsing may be attempted. Thus, the scanner
   recognizes parameter entities and obtains the replacement text from
   a specified entity resolver. These replacement texts, and external
   files such as Dtd are seen as nested files to be scanned until all
   the input file stack is exhausted. *)

IMPORT SGMLC, Rd, RefSeq;

REVEAL
  SGMLC.Scanner <: PublicScanner;

TYPE
  PublicScanner = SGMLC.PublicS OBJECT 
      input: Input;
      inputStack: RefSeq.T;
    METHODS
      initSimple(e: SGMLC.ErrHandler): SGMLC.Scanner;
      setEntityResolver(r: EntityResolver);
      pushState(s: State);
      popState();
      pushFile(name: TEXT; rd: Rd.T);
      pushNextFile(name: TEXT; rd: Rd.T);
      inMarkupDecl(b: BOOLEAN);
    END;

(* Input stores the state for the current file or replacement text, while
   a stack of input states is maintained in case of nested includes or
   parameter entities.

   The call "s.initSimple(e)" initializes "s" with "e" as error handler.

   The call "s.setEntityResolver(r)" sets "r" as the entity resolver from which
   the replacement text may be obtained for parameter entities.

   The call "sc.pushState(s)" pushes "s" as the new state for the 
   non-context-free scanning required by SGML.

   The call "s.popState()" returns to the previous scanning state.

   The call "s.pushFile(name,rd)" inserts "rd" as a new file named "name"
   to read from. Once "rd" is exhausted, the scanning resumes where it was.

   The call "s.pushNextFile(name,rd)" adds "rd" as a file named "name" to
   read from immediately after the current input file is exhausted, before
   resuming with the next input file on the stack.

   The call "s.inMarkupDecl(b)" indicates to the scanner that markup 
   declarations will be processed. *)

  EntityResolver = OBJECT METHODS
      resolve(name: TEXT): Rd.T;
    END;

(* The entity resolver object decouples the scanner from the higher level
   construct (usually the parser) which finds and stores parameter entities
   and their corresponding replacement text. *)

  State = { AttValue, EntityValue, PCData, ContentCSect, StartCSect, 
      ElementTag, DocType, Element, AttList, Entity, Notation, Catalog };

(* The scanning is slightly different for each of these possible states. *)

  Input = REF RECORD
      offset: CARDINAL := 0;
      currentLine: CARDINAL := 1;
      currentCol: CARDINAL := 0;
      rd: Rd.T := NIL;
      name: TEXT := "";
    END;

(* The input structure stores relevant information to determine the exact 
   position in a file, typically for error reporting. *)

END SGMLCScanner.
