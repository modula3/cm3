(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Protocol.i3 *)
(* Last modified on Wed Aug 31 16:41:10 PDT 1994 by wobber  *)
(*      modified on Thu Nov 19 21:31:45 1992 by gnelson *)
(*      modified on Wed Jun 10 17:14:36 PDT 1992 by owicki *)

(* The "Protocol" interface defines header formats of messages
   that encode remote method invocations and stream marshaling.
   Clients who wish to write optimized stubs that operate directly
   on transport connections should use the types in this interface
   to produce the appropriate wire representation. *)
   
INTERFACE Protocol;

IMPORT StubLib, WireRep;

TYPE
  Header = StubLib.DataRep;
  Op = { MethodCall, Return, CallFailed, w1, w2, ResultAck,
            StreamData, StreamClose, StreamRelease,
            StreamOK, StreamError };

(* This header appears in all messages.  The "intFmt", "floatFmt", and
   "charSet" fields of a "Header" indicate the native data representation
   of the sender.   For a header "hdr", "VAL(hdr.private, Op)" indicates
   the message type as follows:

   "MethodCall" indicates a method invocation.  A header of type "CallHeader"
      (below) is expected.  It is followed by the arguments of the call.
      A reponse of type "Return" or "CallFailed" is expected.

   "Return" indicates a successful return from a method invocation.  The
      header is followed by marshaled results.  If the result contains
      a marshaled subtype of "NetObj.T", a reponse of type "ResultAck"
      is expected. 

   "CallFailed" indicates that a remote method invocation failed,
      and that the message contains an "AtomList.T" which specifies the
      failure reason. 

   "ResultAck" indicates an acknowledgement for result marshaling where
      the results included a stubtype of "NetObj.T".  This message
      type has null content.

   "StreamData" indicates that the message contains the characters
      for the source of a remote "Rd.T" or for the target of a
      remote "Wr.T".  In the former case, the following message
      will be of type "StreamOK" or "StreamError" and this will
      indicate success or failure upon reading the concrete reader.
      In the latter case, target "Wr.T" should be flushed after the
      entire message is received and written to the target, and a
      "StreamOK" or "StreamError" response is expected.

   "StreamClose" indicates that the associated concrete "Rd.T" or
      "Wr.T" should be closed.  This message type has null content.
      A response message of "StreamOK" or "StreamError" is expected.

   "StreamRelease" indicates that the resources associated with
      a surrogate "Rd.T" or "Wr.T" should be released without closing
      the concrete stream.  This message type has null content.
      A response message of "StreamOK" or "StreamError" is expected.

   "StreamOK" indicates that no error occured while reading,
      writing, flushing, closing, or releasing the concrete target
      of a surrogate "Rd.T" or "Wr.T".   If the target is a writer,
      this message type is sent in response to a "StreamData" message.
      If the target is a reader, then this message type follows the
      successful marshaling of reader data.  In either case, a "StreamOK"
      message can be sent in response to a "StreamClose" or "StreamRelease"
      message.  The message itself has no content.

   "StreamError" indicates that an error occured while reading,
      writing, flushing, closing, or releasing  the concrete target of
      a surrogate "Rd.T" or "Wr.T".  The message contains an "AtomList.T"
      describing the failure.

   "w1" and "w2" are placeholders for protocol backward compatibility.
*)

  CallHeader = RECORD
    hdr: Header;
    prot: StubLib.StubProtocol;
    obj: WireRep.T;
  END;

(* A "CallHeader" prefixes every message initiating a remote method call.
   It contains a "Header", the stub protocol employed by the invoker's stub
   (in native byte order), and the object ID of the target object. *)

  MsgHeader = RECORD
    hdr: Header;
    pad: StubLib.Int32;
  END;

(* A "MsgHeader" prefixes every message other than a remote method call.
   It is padded to 64-bit alignment so that data streams will be aligned
   correctly in memory on 64-bit machines.
*)

END Protocol.

