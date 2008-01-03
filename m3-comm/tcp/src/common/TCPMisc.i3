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
 * $Id: TCPMisc.i3,v 1.1 2003-07-28 14:12:14 wagner Exp $ *)

(* The "TCPMisc" interface provides miscellaneous TCP-related services. *)

INTERFACE TCPMisc;

IMPORT IP, Thread;

IMPORT TCP;

PROCEDURE AcceptFrom(c: TCP.Connector; VAR (*OUT*) peer: IP.Endpoint): TCP.T
    RAISES {IP.Error, Thread.Alerted};
(* Like TCP.Accept, but returns the peer's address. *)

PROCEDURE CoalesceWrites(tcp: TCP.T; allow: BOOLEAN)
  RAISES {IP.Error};
(* Controls the "TCP_NODELAY" option for the given TCP channel.  If
   "allow" is "TRUE", then "TCP_NODELAY" is turned off.  If "allow" is
   "FALSE", "TCP_NODELAY" is turned on. *)

PROCEDURE ConnectFrom(to, from: IP.Endpoint): TCP.T
  RAISES {IP.Error, Thread.Alerted};
(* Like "TCP.Connect", except it binds the local socket to a specific
   endpoint. *)

PROCEDURE GetPeerName(tcp: TCP.T): IP.Endpoint
  RAISES {IP.Error};
(* Returns the endpoint of the far end of the TCP channel. *)

PROCEDURE GetSockName(tcp: TCP.T): IP.Endpoint
  RAISES {IP.Error};
(* Returns the endpoint of the near end of the TCP channel. *)

PROCEDURE KeepAlive(tcp: TCP.T; allow: BOOLEAN)
  RAISES {IP.Error};
(* Controls the "SO_KEEPALIVE" option for the given TCP channel. *)

PROCEDURE LingerOnClose(tcp: TCP.T; allow: BOOLEAN)
  RAISES {IP.Error};
(* Controls the "SO_LINGER" option for the given TCP channel. *)

END TCPMisc.
