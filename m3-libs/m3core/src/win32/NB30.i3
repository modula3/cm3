(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Thu Apr 13 08:09:19 PDT 1995 by kalsow   *)
(*      modified on Thu Feb 11 13:30:20 PST 1993 by mjordan  *)
(*      modified on Wed Feb 10 21:31:40 PST 1993 by harrison *)

INTERFACE NB30;

(* This module contains the definitions for portable NetBIOS 3.0
   support. *)

IMPORT Ctypes;
FROM WinBaseTypes IMPORT UINT8, PUINT8, UINT16, UINT32, HANDLE;

(****************************************************************
 *                                                              *
 *              Data structure templates                        *
 *                                                              *
 ****************************************************************)

CONST
  NCBNAMSZ = 16;                (* absolute length of a net name *)
  MAX_LANA = 254;               (* lana's in range 0 to MAX_LANA *)

(*
 * Network Control Block
 *)

TYPE
  PNCB = UNTRACED REF NCB;
  NCB = RECORD
    ncb_command  : UINT8;    (* command code *)
    ncb_retcode  : UINT8;    (* return code *)
    ncb_lsn      : UINT8;    (* local session number *)
    ncb_num      : UINT8;    (* number of our network name *)
    ncb_buffer   : PUINT8;   (* address of message buffer *)
    ncb_length   : UINT16;     (* size of message buffer *)
    ncb_callname : NCB_name; (* blank-padded name of remote *)
    ncb_name     : NCB_name; (* our blank-padded netname *)
    ncb_rto      : UINT8;    (* rcv timeout/retry count *)
    ncb_sto      : UINT8;    (* send timeout/sys timeout *)
    ncb_post     : NCB_post; (* POST routine address *)
    ncb_lana_num : UINT8;    (* lana (adapter) number *)
    ncb_cmd_cplt : UINT8;    (* 0xff => commmand pending *)
    ncb_reserve  : ARRAY [0 .. 9] OF UINT8;  (* reserved, used by BIOS *)
    ncb_event    : HANDLE;   (* HANDLE to Win32 event which *)
                             (* will be set to the signalled *)
                             (* state when an ASYNCH command *)
                             (* completes *)
  END;

  NCB_name = ARRAY [0 .. NCBNAMSZ - 1] OF UINT8;
  NCB_post = <*CALLBACK*> PROCEDURE (arg: PNCB);

  (*
   *  Structure returned to the NCB command NCBASTAT is ADAPTER_STATUS followed
   *  by an array of NAME_BUFFER structures.
   *)

  PADAPTER_STATUS = UNTRACED REF ADAPTER_STATUS;
  ADAPTER_STATUS = RECORD
    adapter_address  : ARRAY [0 .. 6 - 1] OF UINT8;
    rev_major        : UINT8;
    reserved0        : UINT8;
    adapter_type     : UINT8;
    rev_minor        : UINT8;
    duration         : UINT16;
    frmr_recv        : UINT16;
    frmr_xmit        : UINT16;

    iframe_recv_err  : UINT16;

    xmit_aborts      : UINT16;
    xmit_success     : UINT32;
    recv_success     : UINT32;

    iframe_xmit_err  : UINT16;

    recv_buff_unavail: UINT16;
    t1_timeouts      : UINT16;
    ti_timeouts      : UINT16;
    reserved1        : UINT32;
    free_ncbs        : UINT16;
    max_cfg_ncbs     : UINT16;
    max_ncbs         : UINT16;
    xmit_buf_unavail : UINT16;
    max_dgram_size   : UINT16;
    pending_sess     : UINT16;
    max_cfg_sess     : UINT16;
    max_sess         : UINT16;
    max_sess_pkt_size: UINT16;
    name_count       : UINT16;
  END;

  PNAME_BUFFER = UNTRACED REF NAME_BUFFER;
  NAME_BUFFER = RECORD
    name      : NCB_name;
    name_num  : UINT8;
    name_flags: UINT8;
  END;

(* values for name_flags bits. *)

CONST
  NAME_FLAGS_MASK = 16_87;

  GROUP_NAME  = 16_80;
  UNIQUE_NAME = 16_00;

  REGISTERING     = 16_00;
  REGISTERED      = 16_04;
  DEREGISTERED    = 16_05;
  DUPLICATE       = 16_06;
  DUPLICATE_DEREG = 16_07;

(*
 *  Structure returned to the NCB command NCBSSTAT is SESSION_HEADER followed
 *  by an array of SESSION_BUFFER structures. If the NCB_NAME starts with an
 *  asterisk then an array of these structures is returned containing the
 *  status for all names.
 *)

TYPE
  PSESSION_HEADER = UNTRACED REF SESSION_HEADER;
  SESSION_HEADER = RECORD
    sess_name           : UINT8;
    num_sess            : UINT8;
    rcv_dg_outstanding  : UINT8;
    rcv_any_outstanding : UINT8;
  END;

  PSESSION_BUFFER = UNTRACED REF SESSION_BUFFER;
  SESSION_BUFFER = RECORD
    lsn               : UINT8;
    state             : UINT8;
    local_name        : NCB_name;
    remote_name       : NCB_name;
    rcvs_outstanding  : UINT8;
    sends_outstanding : UINT8;
  END;

(* Values for state *)

CONST
  LISTEN_OUTSTANDING  = 16_01;
  CALL_PENDING        = 16_02;
  SESSION_ESTABLISHED = 16_03;
  HANGUP_PENDING      = 16_04;
  HANGUP_COMPLETE     = 16_05;
  SESSION_ABORTED     = 16_06;

(*
 *  Structure returned to the NCB command NCBENUM.
 *
 *  On a system containing lana's 0, 2 and 3, a structure with
 *  length =3, lana[0]=0, lana[1]=2 and lana[2]=3 will be returned.
 *)

TYPE
  PLANA_ENUM = UNTRACED REF LANA_ENUM;
  LANA_ENUM = RECORD
    length: UINT8;  (* Number of valid entries in lana[] *)
    lana: ARRAY [0 .. MAX_LANA] OF UINT8;
  END;

  (*
   *  Structure returned to the NCB command NCBFINDNAME is FIND_NAME_HEADER
   *  followed by an array of FIND_NAME_BUFFER structures.
   *)

  PFIND_NAME_HEADER = UNTRACED REF FIND_NAME_HEADER;
  FIND_NAME_HEADER = RECORD
    node_count  : UINT16;
    reserved    : UINT8;
    unique_group: UINT8;
  END;

  PFIND_NAME_BUFFER = UNTRACED REF FIND_NAME_BUFFER;
  FIND_NAME_BUFFER = RECORD
    length          : UINT8;
    access_control  : UINT8;
    frame_control   : UINT8;
    destination_addr: ARRAY [0 .. 6 - 1] OF UINT8;
    source_addr     : ARRAY [0 .. 6 - 1] OF UINT8;
    routing_info    : ARRAY [0 .. 18 - 1] OF UINT8;
  END;

  (*
   *  Structure provided with NCBACTION. The purpose of NCBACTION is to provide
   *  transport specific extensions to netbios.
   *)

  PACTION_HEADER = UNTRACED REF ACTION_HEADER;
  ACTION_HEADER = RECORD
    transport_id: UINT32;
    action_code : UINT16;
    reserved    : UINT16;
  END;

(* Values for transport_id *)

VAR (* CONST *)
  ALL_TRANSPORTS: Ctypes.char_star;
  MS_NBF        : Ctypes.char_star;


(****************************************************************
 *                                                              *
 *              Special values and constants                    *
 *                                                              *
 ****************************************************************)

(*
 *      NCB Command codes
 *)

CONST
  NCBCALL        = 16_10;       (* NCB CALL *)
  NCBLISTEN      = 16_11;       (* NCB LISTEN *)
  NCBHANGUP      = 16_12;       (* NCB HANG UP *)
  NCBSEND        = 16_14;       (* NCB SEND *)
  NCBRECV        = 16_15;       (* NCB RECEIVE *)
  NCBRECVANY     = 16_16;       (* NCB RECEIVE ANY *)
  NCBCHAINSEND   = 16_17;       (* NCB CHAIN SEND *)
  NCBDGSEND      = 16_20;       (* NCB SEND DATAGRAM *)
  NCBDGRECV      = 16_21;       (* NCB RECEIVE DATAGRAM *)
  NCBDGSENDBC    = 16_22;       (* NCB SEND BROADCAST DATAGRAM *)
  NCBDGRECVBC    = 16_23;       (* NCB RECEIVE BROADCAST DATAGRAM *)
  NCBADDNAME     = 16_30;       (* NCB ADD NAME *)
  NCBDELNAME     = 16_31;       (* NCB DELETE NAME *)
  NCBRESET       = 16_32;       (* NCB RESET *)
  NCBASTAT       = 16_33;       (* NCB ADAPTER STATUS *)
  NCBSSTAT       = 16_34;       (* NCB SESSION STATUS *)
  NCBCANCEL      = 16_35;       (* NCB CANCEL *)
  NCBADDGRNAME   = 16_36;       (* NCB ADD GROUP NAME *)
  NCBENUM        = 16_37;       (* NCB ENUMERATE LANA NUMBERS *)
  NCBUNLINK      = 16_70;       (* NCB UNLINK *)
  NCBSENDNA      = 16_71;       (* NCB SEND NO ACK *)
  NCBCHAINSENDNA = 16_72;       (* NCB CHAIN SEND NO ACK *)
  NCBLANSTALERT  = 16_73;       (* NCB LAN STATUS ALERT *)
  NCBACTION      = 16_77;       (* NCB ACTION *)
  NCBFINDNAME    = 16_78;       (* NCB FIND NAME *)
  NCBTRACE       = 16_79;       (* NCB TRACE *)


  ASYNCH = 16_80;               (* high bit set == asynchronous *)

(*
 *      NCB Return codes
 *)

CONST
  NRC_GOODRET = 16_00;          (* good return *)
  (* also returned when ASYNCH request accepted *)
  NRC_BUFLEN = 16_01;           (* illegal buffer length *)
  NRC_ILLCMD = 16_03;           (* illegal command *)
  NRC_CMDTMO = 16_05;           (* command timed out *)
  NRC_INCOMP = 16_06;           (* message incomplete, issue another
                                   command *)
  NRC_BADDR   = 16_07;          (* illegal buffer address *)
  NRC_SNUMOUT = 16_08;          (* session number out of range *)
  NRC_NORES   = 16_09;          (* no resource available *)
  NRC_SCLOSED = 16_0a;          (* session closed *)
  NRC_CMDCAN  = 16_0b;          (* command cancelled *)
  NRC_DUPNAME = 16_0d;          (* duplicate name *)
  NRC_NAMTFUL = 16_0e;          (* name table full *)
  NRC_ACTSES = 16_0f;           (* no deletions, name has active
                                   sessions *)
  NRC_LOCTFUL = 16_11;          (* local session table full *)
  NRC_REMTFUL = 16_12;          (* remote session table full *)
  NRC_ILLNN   = 16_13;          (* illegal name number *)
  NRC_NOCALL  = 16_14;          (* no callname *)
  NRC_NOWILD  = 16_15;          (* cannot put * in NCB_NAME *)
  NRC_INUSE   = 16_16;          (* name in use on remote adapter *)
  NRC_NAMERR  = 16_17;          (* name deleted *)
  NRC_SABORT  = 16_18;          (* session ended abnormally *)
  NRC_NAMCONF = 16_19;          (* name conflict detected *)
  NRC_IFBUSY  = 16_21;          (* interface busy, IRET before retrying *)
  NRC_TOOMANY = 16_22;          (* too many commands outstanding, retry
                                   later *)
  NRC_BRIDGE = 16_23;           (* ncb_lana_num field invalid *)
  NRC_CANOCCR = 16_24;          (* command completed while cancel
                                   occurring *)
  NRC_CANCEL = 16_26;           (* command not valid to cancel *)
  NRC_DUPENV = 16_30;           (* name defined by anther local process *)
  NRC_ENVNOTDEF = 16_34;        (* environment undefined.  RESET
                                   required *)
  NRC_OSRESNOTAV  = 16_35;      (* required OS resources exhausted *)
  NRC_MAXAPPS     = 16_36;      (* max number of applications exceeded *)
  NRC_NOSAPS      = 16_37;      (* no saps available for netbios *)
  NRC_NORESOURCES = 16_38;      (* requested resources are not available *)
  NRC_INVADDRESS = 16_39;       (* invalid ncb address or length >
                                   segment *)
  NRC_INVDDID  = 16_3B;         (* invalid NCB DDID *)
  NRC_LOCKFAIL = 16_3C;         (* lock of user area failed *)
  NRC_OPENERR  = 16_3f;         (* NETBIOS not loaded *)
  NRC_SYSTEM   = 16_40;         (* system error *)

  NRC_PENDING = 16_ff;          (* asynchronous command is not yet
                                   finished *)

(****************************************************************
 *                                                              *
 *              main user entry point for NetBIOS 3.0           *
 *                                                              *
 * Usage: result = Netbios( pncb );                             *
 ****************************************************************)

<*EXTERNAL Netbios:WINAPI*>
PROCEDURE Netbios(pncb: PNCB): UINT8;

END NB30.
