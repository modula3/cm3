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

MODULE SplitLogger;

IMPORT Logger, LoggerClass;

REVEAL
  T = Public BRANDED OBJECT
    child1, child2: Logger.T;
    levels1, levels2: SET OF Logger.Priority;
  OVERRIDES
    init := Init;
    put := Put;
    close := Close;
  END;

PROCEDURE Init(self: T;
	       child1, child2: Logger.T;
	       levels1 := SET OF Logger.Priority{
		  Logger.Priority.Emerg .. Logger.Priority.Warning};
	       levels2 := SET OF Logger.Priority{
		  Logger.Priority.Notice .. Logger.Priority.Debug}): T =
  BEGIN
    EVAL Logger.T.init(self);
    self.child1 := child1;
    self.child2 := child2;
    self.levels1 := levels1;
    self.levels2 := levels2;
    RETURN self;
  END Init;

PROCEDURE Put(self: T;
              priority: Logger.Priority;
	      msg: TEXT) =
  BEGIN
    IF self.child1 # NIL AND priority IN self.levels1 THEN
      Logger.Put(self.child1, priority, msg);
    END;
    IF self.child2 # NIL AND priority IN self.levels2 THEN
      Logger.Put(self.child2, priority, msg);
    END;
  END Put;

PROCEDURE Close(self: T) =
  BEGIN
    IF self.child1 # NIL THEN
      Logger.Close(self.child1);
    END;
    IF self.child2 # NIL THEN
      Logger.Close(self.child2);
    END;
  END Close;

BEGIN
END SplitLogger.
