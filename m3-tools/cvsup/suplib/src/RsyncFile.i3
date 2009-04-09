(* Copyright 1996-2003 John D. Polstra.
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

INTERFACE RsyncFile;

IMPORT FileAttr, OSError, Pathname, RsyncBlock, Thread, Wr;

TYPE
  T <: Public;

  Public = OBJECT
    (* These are all READONLY. *)
    attr: FileAttr.T;
    blockSize: CARDINAL;
  END;

PROCEDURE Open(p: Pathname.T;
               blockSize: CARDINAL := 0): T
  RAISES {OSError.E};

(* If "blockSize" is specified, it will be used exactly.  Otherwise, a
   suitable value will be chosen. *)

PROCEDURE Close(rf: T)
  RAISES {OSError.E};

PROCEDURE GetMD5(rf: T): TEXT;
(* Return the MD5 checksum of the entire file in text form. *)

(* Generating the checksums from the file to be updated. *)

TYPE
  BlockIterator = OBJECT METHODS
    next(VAR block: RsyncBlock.T): BOOLEAN;
  END;

PROCEDURE IterateBlocks(rf: T): BlockIterator;

(* Generating the diffs from the master file. *)

TYPE
  BlockRange = RECORD
    start: CARDINAL;   (* First block. *)
    count: CARDINAL;   (* Number of blocks. *)
  END;

  DiffIterator = OBJECT METHODS
    next(wr: Wr.T; VAR blocks: BlockRange): BOOLEAN
      RAISES {Thread.Alerted, Wr.Failure};
  END;

PROCEDURE IterateDiffs(rf: T;
                       blocks: REF ARRAY OF RsyncBlock.T): DiffIterator;

END RsyncFile.
