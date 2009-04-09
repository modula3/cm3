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

(* A "SupFileRec.T" represents one record (i.e., line) from a supfile. *)

INTERFACE SupFileRec;

IMPORT
  ExecRecSeq, FileAttr, GlobTree, Pathname, RCSKeyword, TextSeq, Time,
  TokScan;

CONST Brand = "SupFileRec";

TYPE
  T <: Public;

  Public = MUTEX OBJECT
    collection: TEXT;
    release: TEXT;
    serverHost: TEXT;          (* Hostname of server machine. *)
    clientBase: Pathname.T;    (* Root of client's sup database. *)
    serverBase: Pathname.T;    (* Root of host's sup database. *)
    clientCollDir: Pathname.T; (* Client base subdirectory for collections. *)
    serverCollDirs: TEXT;      (* Server base subdirectory search path
				  for collections. *)
    clientPrefix: Pathname.T;  (* Root of tree on client side. *)
    serverPrefix: Pathname.T;  (* Root of tree on server side. *)
    keywordPrefix: Pathname.T; (* Logical root for RCS keyword expansion. *)
    serverListFile: Pathname.T;  (* Path of "list" file on server. *)
    serverScanFile: Pathname.T;  (* Path of scan file on server. *)
    superCollection: TEXT;     (* Name of immediate superset collection. *)
    checkoutTag: TEXT;         (* Desired symbolic tag for checkout mode. *)
    checkoutDate: TEXT;        (* Desired date for checkout mode, RCS format. *)
    listSuffix: TEXT;          (* List file suffix for checkout mode. *)
    options: Options;          (* See below. *)
    scanTime: Time.T;          (* When server began its tree walk. *)
    accepts: TextSeq.T;        (* Client-side acceptance list. *)
    refusals: TextSeq.T;       (* Client-side refusal list. *)
    executes: ExecRecSeq.T;    (* Commands to execute for specific files. *)
    fileFilter: GlobTree.T;    (* File acceptance filter. *)
    dirFilter: GlobTree.T;     (* Directory acceptance filter. *)
    symlink: GlobTree.T;       (* Filter for transferring symlinks *)
    noRsync: GlobTree.T;       (* Filter for suppressing rsync mode *)
    attrIgnore: FileAttr.AttrTypes;  (* Attributes that should be ignored. *)
    umask: INTEGER;            (* Client's assumed umask for this collection,
				  or -1 if unknown. *)
    expander: RCSKeyword.T;    (* RCS keyword expander. *)
    bytesIn, bytesOut := 0.0d0;  (* Network I/O statistics. *)
  METHODS
    init(default: T := NIL): T;
    overrideFrom(v: T; mask := Options{});
  END;

(* The "init" method initializes all fields to reasonable starting
   values.  If the "default" parameter is specified, the
   client-controlled fields are initialized from it.

   The "overrideFrom" method overrides some client-controlled fields
   of "self" with values taken from "v".  Only the non-default
   fields of "v" are used to override the corresponding fields of
   "self".  One exception:  the "accepts" and "refusals" sequences
   are concatenated.

   Only the options present in "mask" are overridden; the
   others remain the same. *)

  Options = SET OF Option;

(* Note:  Add new options only at the end.  Do not reorder or delete
   existing options.  If you do, you'll break compatibility with earlier
   versions of the protocol. *)

  Option = {
    Backup,         (* Create a backup of each upgraded file (not impl.) *)
    Delete,         (* Delete files no longer present on server *)
    Keep,           (* Don't update files newer on client (not impl.) *)
    Old,            (* Check all files instead of just new ones (not impl.) *)
    UnlinkBusy,     (* Unlink ETXTBSY files before upgrading (not impl.) *)
    NoUpdate,       (* Don't update file if modtime is unchanged (not impl.) *)
    Compress,       (* Use compressed transfers. *)
    UseRelSuffix,   (* Suffix "checkouts" files with release and tag. *)
    ExactRCS,       (* Keep RCS files exactly as on server -- no local mods. *)
    CheckRCS,       (* Checksum RCS files to make sure they're correct. *)
    Skip,           (* Skip this collection, because of errors. *)
    CheckoutMode,   (* Using checkout mode. *)
    NoRsync,        (* Don't use rsync algorithm to update regular files. *)
    KeepBadFiles,   (* Keep the bad files when fixups are required. *)
    Execute,        (* Allow client execution of commands from server *)
    SetOwner,       (* Propagate owner/group of files. *)
    SetMode,        (* Propagate permissions of files. *)
    SetFlags,       (* Propagate flags of files. *)
    NoRCS,          (* Treat RCS files the same as all other files. *)
    StrictCheckRCS, (* Checksum RCS files byte-for-byte instead of logically. *)
    TrustStatusFile,(* Don't stat client files to check them against the
		       status file. *)
    DoDeletesOnly,  (* Omit all updates except file deletions. *)
    DetailAllRCSFiles (* Detail all RCS files even if file attributes are equal *)
  };

CONST AllOptions = Options{FIRST(Option)..LAST(Option)};

PROCEDURE Compare(a, b: T): [-1..1];
(* Compares by collection name, then by release name. *)

PROCEDURE Equal(a, b: T): BOOLEAN;
(* Tests whether two "T"'s represent the same collection and release. *)

PROCEDURE EncodeOptions(o: Options): TEXT;
(* Produces a printable representation of the given options. *)

PROCEDURE DecodeOptions(t: TEXT): Options RAISES {TokScan.Error};
(* Converts a printable representation into options. *)

PROCEDURE Check(sfr: T; coll, rel: TEXT) RAISES {TokScan.Error};
(* Raises an exception if the supfile record does not match the given
   collection and release. *)

END SupFileRec.
