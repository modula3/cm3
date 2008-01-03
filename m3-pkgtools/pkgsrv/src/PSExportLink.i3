(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PSExportLink.i3 *)
(* Last modified on Thu May 27 15:16:57 PDT 1993 by wobber *)

INTERFACE PSExportLink;

IMPORT FileSys, PackageObj, PkgErr, Rd, Wr, Thread;

TYPE
  ExportLinks = PackageObj.ExportLinks;
  Monitor = PackageObj.Monitor;

TYPE
  WriteActions <: REFANY;


PROCEDURE Read (rd: Rd.T): ExportLinks
    RAISES {Rd.Failure, Thread.Alerted};
  (* "rd" is a reader on the export link file *)

PROCEDURE Prepare(
    pn: PackageObj.PN; fn: FileSys.FN; oldLinks, links: ExportLinks;
    purge: BOOLEAN; mon: Monitor) : WriteActions
    RAISES {PkgErr.E, Thread.Alerted};

  (* "pn" is the name of the package at hand.
     "fn" is the fs path to which the package name maps.
     "oldlinks" are the set of links previously shipped.
     "links" are the set of links to be shipped.
     "purge" indicates whether previous links should be deleted if
        no longer in "links". *)

PROCEDURE Write(actions: WriteActions; wr: Wr.T)
    RAISES {PkgErr.E, Wr.Failure, Thread.Alerted};
  (* argument should be the result of Prepare *)
  (* "wr" is a writer on the new export links file *)

END PSExportLink.

