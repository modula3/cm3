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
 * Created On      : Tue Aug  8 16:29:55 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sun Feb 18 15:02:20 1996
 * Update Count    : 24
 *
 * SCCS Status     : %W%	%G%
 * 
 * HISTORY
 *)

(* TODO: create picklers that will not duplicate the obliq object when
   passed to a host more than once.  Simply send a receive the real
   object first.  If it already exists, there will be a proxy, which
   can just be used. *)

MODULE ObSharedObj;

IMPORT ObCommand, ObLib, ObLoader, ObValue, Obliq, Env,
       ObEmbProxiedObj, SynLocation, SharedObj, SharedObjRT, Text,
       ObText, ObInt, Thread, IP, ObjectSpace, ObError;

(* When a new proxied object is created, we first want to check to see
   if there already is a proxied obj.  If so, just use it.
   This can happen if the obliq value is pickled across the network.
   If the shared object exists, it will be returned from the pickle
   reader.  Thus, we should use the existing proxy, too. *)

REVEAL 
  T = ObEmbProxiedObj.T BRANDED "ObSharedObj.T" OBJECT END;

PROCEDURE GetArg (args    : ObValue.ArgArray; 
                  idx     : INTEGER; 
                  package : ObLib.T; 
                  opCode  : ObLib.OpCode; 
                  loc     : SynLocation.T) : SharedObj.T
    RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    WITH raw = Obliq.ObjectSelect (args[idx], "raw") DO
      TYPECASE raw OF
      | T (node) =>
        RETURN node.po;
      ELSE 
        ObValue.BadArgType (idx, pkgname, package.name, opCode.name, loc);
        RETURN NIL;      (* ... only to suppress compiler warning *)
      END;
    END;
  END GetArg; 

(*****************************************************************************)
(* Setup procedures                                                          *)
(*****************************************************************************)

PROCEDURE SetupPackage() =
  PROCEDURE NewOpCode (name: TEXT; arity: INTEGER; code: Code) : OpCode =
    BEGIN
      RETURN NEW (OpCode, name := name, arity := arity, code := code);
    END NewOpCode;

  VAR opCodes: REF ObLib.OpCodes;
  BEGIN
    opCodes := NEW(REF ObLib.OpCodes, NUMBER(Code));
    opCodes^ :=
        ObLib.OpCodes{
          NewOpCode ("Error",              -1, Code.Error),
          NewOpCode ("Fatal",              -1, Code.Fatal),
          NewOpCode ("AcquireGlobalLock",   1, Code.AcquireGlobalLock),
          NewOpCode ("ReleaseGlobalLock",   1, Code.ReleaseGlobalLock),
          NewOpCode ("Own",                 2, Code.Own),
          NewOpCode ("Disown",              1, Code.Disown),
          NewOpCode ("SetTimeliness",       2, Code.SetTimeliness),
          NewOpCode ("SetNodeName",         1, Code.SetNodeName),
          NewOpCode ("SetDefaultSequencer", 2, Code.SetDefaultSequencer),
          NewOpCode ("DebugLevel",          1, Code.DebugLevel),
          NewOpCode ("PullObject",          1, Code.PullObject)
        };
    errorException := Obliq.NewException("SharedObj_Error");
    fatalException := Obliq.NewException("SharedObj_Fatal");
    ObLib.Register(NEW(Package, name:=pkgname, opCodes:=opCodes));
    ObLib.RegisterHelp (pkgname, Help);
  END SetupPackage;

PROCEDURE SetupModule (loader: ObLoader.T) =
  BEGIN
    pkgloader := loader;
    pkgloader.load ("SharedObj.obl");
  END SetupModule;

(*****************************************************************************)
(* Execution machinery                                                       *)
(*****************************************************************************)

TYPE
  Code = {Error, Fatal, AcquireGlobalLock, ReleaseGlobalLock, Own,
          Disown, SetTimeliness, SetNodeName, SetDefaultSequencer,
          DebugLevel, PullObject}; 

  OpCode = ObLib.OpCode OBJECT
    code: Code;
  END;

  Package = ObLib.T OBJECT OVERRIDES
    Eval := Eval;
  END;

CONST
  pkgname = "SharedObj";

VAR
  nodename: TEXT;


(* PullObject uses a very cute trick.
   The theory is this.  By obtaining the "raw" field, and
   from it the proxy, if the object is remote we will now have
   a locate copy, created when the modula-3 object is pickled
   across, a new one is created and the appropriate mkproxy is
   called.   If the object is locate, we will return the same
   obliq object as before.

   Unlike the anim3D ObProxiedObj.Extend, we do not allow the
   object to be extended: the various copies would them
   potentially become different.

   One of the two methods will end up being more useful.  I'll kill
   one eventually.
 *)

PROCEDURE Eval(self: Package; opCode: ObLib.OpCode;
               <* UNUSED *> arity        : ObLib.OpArity; 
               READONLY args: ObValue.ArgArray; 
               <* UNUSED *> temp: BOOLEAN; loc: SynLocation.T)
    : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    TRY
      CASE NARROW(opCode, OpCode).code OF
      | Code.Error =>
        RETURN errorException;
      | Code.Fatal =>
        RETURN fatalException;
      | Code.AcquireGlobalLock =>
        WITH obj = GetArg(args, 1, self, opCode, loc) DO
          SharedObj.AcquireGlobalLock(obj);
        END;
        RETURN ObValue.valOk;
      | Code.ReleaseGlobalLock =>
        WITH obj = GetArg(args, 1, self, opCode, loc) DO
          SharedObj.ReleaseGlobalLock(obj);
        END;
        RETURN ObValue.valOk;
      | Code.Own =>
        WITH obj = GetArg(args, 1, self, opCode, loc),
             arg1 = ObInt.GetArg(args, 2, self, opCode, loc) DO
          IF arg1 < FIRST(SharedObj.Timeliness) OR 
            arg1 > LAST(SharedObj.Timeliness) THEN
            ObValue.BadArgVal(2, "invalid timeliness", self.name,
                              opCode.name, loc); 
          END;
          SharedObj.Own(obj, arg1);
        END;
        RETURN ObValue.valOk;
      | Code.Disown =>
        WITH obj = GetArg(args, 1, self, opCode, loc) DO
          SharedObj.Disown(obj);
        END;
        RETURN ObValue.valOk;
      | Code.SetTimeliness =>
        WITH obj = GetArg(args, 1, self, opCode, loc),
             arg1 = ObInt.GetArg(args, 2, self, opCode, loc) DO
          IF arg1 < FIRST(SharedObj.Timeliness) OR 
            arg1 > LAST(SharedObj.Timeliness) THEN
            ObValue.BadArgVal(2, "invalid timeliness", self.name,
                              opCode.name, loc); 
          END;
          SharedObj.SetTimeliness(obj, arg1);
        END;
        RETURN ObValue.valOk;
      | Code.SetNodeName =>
        nodename := ObText.GetArg(args, 1, self, opCode, loc);
        IF Text.Equal(nodename, "") THEN
          nodename := IP.GetCanonicalByAddr(IP.GetHostAddr());
        END;
        SharedObjRT.ExportSpace(nodename);
        RETURN ObText.M3ToObliq(nodename);
      | Code.SetDefaultSequencer =>
        VAR space: ObjectSpace.T;
            host := ObText.GetArg(args, 1, self, opCode, loc);
            name := ObText.GetArg(args, 2, self, opCode, loc);
        BEGIN
          IF Text.Equal("", host) THEN
            WITH defhost = Env.Get("SEQUENCERHOST") DO
              IF defhost # NIL THEN
                host := defhost;
              END;
            END;
          END;
          IF Text.Equal("", name) THEN
            WITH defname = Env.Get("SEQUENCERNAME") DO
              IF defname # NIL THEN
                name := defname;
              END;
            END;
          END;

          IF NOT Text.Equal("", host) OR NOT Text.Equal("", name) THEN
            space := SharedObjRT.ImportSpace(host, name);
            IF space = NIL THEN
              ObValue.RaiseException(errorException, 
                                     self.name & "_" & opCode.name & 
                                     ": node " & name & "@" & host &
                                     " is unavailable", loc);
            END;
          ELSE
            space := SharedObjRT.LocalSpace();
          END;
          SharedObjRT.SetDfltSequencer(space);
        END;
        RETURN ObValue.valOk;
      | Code.DebugLevel =>
        WITH arg1 = ObInt.GetArg(args, 1, self, opCode, loc) DO
          SharedObjRT.DebugLevel(arg1);
        END;
        RETURN ObValue.valOk;
      | Code.PullObject =>
        WITH po = GetArg (args, 1, self, opCode, loc) DO
          RETURN po.proxy.obj;
        END;
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc);
        RETURN NIL; (* to shut up warning in compiler about no return value *)
      END;
    EXCEPT
    | IP.Error(ec) => 
      ObValue.RaiseException(ObValue.netException, 
                             self.name & "_" & opCode.name & 
                             ": " & ObError.AtomListToText(ec), loc);
      RETURN NIL; (* to shut up warning in compiler about no return value *)
    | Thread.Alerted =>
      ObValue.RaiseException(ObValue.threadAlerted, 
                             self.name & "_" & opCode.name & ": ", loc);
      RETURN NIL; (* to shut up warning in compiler about no return value *)
    | SharedObj.Error(ec) =>
      ObValue.RaiseException(errorException, 
                             self.name & "_" & opCode.name & 
                             ": " & ObError.AtomListToText(ec), loc);
      RETURN NIL; (* to shut up warning in compiler about no return value *)
    END;
  END Eval;
  
(*****************************************************************************)
(* Help                                                                      *)
(*****************************************************************************)

PROCEDURE Help (self : ObCommand.T; arg : TEXT; <* UNUSED *> data : REFANY) =
  BEGIN
    IF pkgloader # NIL THEN
      pkgloader.help (self, arg, pkgname);
    END;
  END Help;

VAR 
  pkgloader: ObLoader.T := NIL;

BEGIN
END ObSharedObj. 
