(* Copyright 1997-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$ *)

INTERFACE Receive;

(* Subroutines used by the various file updaters for receiving file data
   from the network. *)

IMPORT
  RCSDelta, RCSError, RCSFile, RCSRevNum, Rd, Thread, TokScan, Wr;

EXCEPTION Error(TEXT);

PROCEDURE Counted(rd: Rd.T;
                  wr: Wr.T;
		  size: CARDINAL;
		  withChecksum: BOOLEAN): TEXT
  RAISES {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error,
	  Wr.Failure};
(* Receives a counted transfer from the server, possibly followed by a
   checksum.  If "withChecksum" is "TRUE", then the checksum is
   read and returned.  Otherwise, "NIL" is returned.  The "Error"
   exception is raised if the file size changed on the server.
   Protocol errors cause "TokScan.Error" exceptions. *)

PROCEDURE Delta(rd: Rd.T;
                rf: RCSFile.T;
		revNum: RCSRevNum.T;
		diffBaseRev: RCSRevNum.T;
		date: TEXT;
		author: TEXT): RCSDelta.T
  RAISES {RCSError.E, Rd.EndOfFile, Rd.Failure, Thread.Alerted,
	  TokScan.Error};
(* Receives a delta from the server, and adds it to an RCS file. *)

PROCEDURE Escaped(rd: Rd.T;
                  wr: Wr.T;
		  withChecksum: BOOLEAN): TEXT
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted, TokScan.Error,
	  Wr.Failure};
(* Receives some escaped text from the server, possibly followed by a
   checksum.  If "withChecksum" is "TRUE", then the checksum is read
   and returned.  Otherwise, "NIL" is returned. *)

END Receive.
