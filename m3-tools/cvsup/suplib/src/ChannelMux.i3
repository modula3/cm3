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

INTERFACE ChannelMux;

IMPORT Atom, IP, Rd, Thread, Wr;
IMPORT ConnFD;

TYPE
  T <: ROOT;

  Channel <: ConnFD.T;

  ChannelID = [0..255];

VAR
(* Error atoms in addition to those defined in the "IP" and "TCP"
   interfaces. *)
  ProtocolError: Atom.T;

PROCEDURE Open(rd: Rd.T;
               wr: Wr.T;
	       VAR (*OUT*) chan: Channel;
	       active: BOOLEAN): T
  RAISES {IP.Error, Thread.Alerted};
(* Creates and initializes a new channel multiplexer communicating on
   top of the given reader/writer pair. *)

PROCEDURE Listen(mux: T): ChannelID
  RAISES {IP.Error};
(* Creates a new channel in the listening state, and returns its ID. *)

PROCEDURE Accept(mux: T; id: ChannelID): Channel
  RAISES {IP.Error, Thread.Alerted};
(* Waits for an incoming connection on the given channel. *)

PROCEDURE Connect(mux: T; id: ChannelID): Channel
  RAISES {IP.Error, Thread.Alerted};
(* Initiates a connection to the given channel. *)

PROCEDURE Close(mux: T);
(* Shuts down the packet multiplexer. *)

END ChannelMux.
