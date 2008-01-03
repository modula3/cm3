(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:13:16 PDT 1996 by mhb       *)

INTERFACE DocVBT;

(* A DocVBT.T is a document.  Each DeckVBT contains a collection of
   DocVBTs, and each FreeDocVBT contains one DocVBT.  In turn, each DocVBT
   displays the contents of an Web page. *)

IMPORT FormsVBT, VBT, Web;

TYPE
  T <: FormsVBT.T;

CONST NumberOfExpansionThreads = 2;

VAR DefaultHomeURL: TEXT;
  (* Set to the contents of the environment variable WWW_HOME;
     if that is not define or is empty, then defaults to 
     "http://www.research.digital.com/". *)

PROCEDURE NewFromURL (url: TEXT := NIL; reload := FALSE; fork := TRUE): T;
(* Return a new "DocVBT.T" displaying the URL "url".  (Defaults to 
   "DefaultHomeURL". *)

PROCEDURE NewFromPage(page: Web.Page; base: TEXT): T;
(* Return a new "DocVBT.T" displaying the Web "page" whose
  base URL is "base". *)

PROCEDURE Search(doc: T; text: TEXT): BOOLEAN; 
(* Returns whether "text" is contained in "doc". *)

PROCEDURE Copy(doc: T): T;
(* Return a copy of "doc". *)

PROCEDURE Reload (doc: T): T;
(* Return a reloaded copy of "doc", ready to be installed in a free-doc or
   a deck. *)

PROCEDURE SetOwner(doc: T; owner: VBT.T); 
(* Must be called whenever "doc" is moved to a different deck or
   free-doc. *)

PROCEDURE GetOwner(doc: T): VBT.T; 
(* Return "doc"'s owner, either a deck or a free-doc *)

PROCEDURE GetTitle (doc: T): TEXT;
(* Return the title field of the "doc".  Some heuristics are used to make a
   guess at the title. *)

PROCEDURE GetPage(doc: T): Web.Page;
(* Return the Web page that is displayed by "doc". *)

PROCEDURE Expand (doc: T): VBT.T;
(* Return a new deck, installed in the Workspace, with all Web pages that
   "doc" references.  When this procedure returns, the deck is empty.  A
   thread is forked to fetch all of the references.  When this thread
   completes, the banner of the deck is changed to indicate this. *)

END DocVBT.

