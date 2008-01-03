(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Thu Apr 21 10:44:42 PDT 1994 by birrell*)
(*      modified on Mon Jul  6 17:52:30 PDT 1992 by meehan*)
(*      modified on Sat Jul  4 02:19:10 1992 by mhb     *)

(*      modified on Fri Mar 10 17:31:39 PST 1989 by denning *)
(*      modified on Thu Jun 30 20:43:12 1988 by mcjones *)
(*      modified on Wed Dec 23 11:01:24 PST 1987 by discolo *)


INTERFACE UnixMail;

IMPORT Closure, TextList, Rd, Text, Time, Wr;

EXCEPTION
  Error (Text.T);


TYPE
  FolderPtr <: REFANY;

CONST
  NIFolderName = "/NIFolder"; (* arbitrary - identifies NI browser folder *)
  InboxName = "inbox";

(* CONCURRENCY NOTES
   The procedures in this interface are correctly synchronized for concurrent
   calling, in the sense that they will maintain the appropriate invariants on
   the file system and the internal data structures. However, most of them
   should be serialized by the client to provide human sensible behavior. *)


(*********************)
(* Running processes *)
(*********************)

PROCEDURE QueueFilterFile(pathName: TEXT; filter: TEXT) RAISES { Error };
  (* Run the filter with "pathName" as its final argument. The process
     will be run asynchronously in the general background queue, serialized
     with other background queue operations.  The child's stdin is a pipe that
     has been closed.  Any output created by the child is discarded unless the
     child terminates with non-zero status (or by dumping core), in which
     case the output is appended to the error message. *)

PROCEDURE RunFilterFile(pathName: TEXT; filter: TEXT) RAISES { Error };
  (* Run the filter with "pathName" as its final argument.  The process
     is run synchronously; this thread blocks until the process exits.
     The child's stdin, stdout and stderr are as for QueueFilterFile. *)

PROCEDURE RunFilterText(t: TEXT; from: INTEGER; filter: TEXT) RAISES { Error };
  (* Same as RunFilterFile, but using a temporary file containing the
     given sub-text.  The file is deleted when the child process
     terminates. *)


(*******************)
(* Folder routines *)
(*******************)

PROCEDURE FindFolder(folder: Text.T): FolderPtr RAISES {Error};
  (* Returns a handle suitable for GetNextFolder *)

PROCEDURE GetNextFolder(folderPtr: FolderPtr; forward: BOOLEAN;
                        VAR folder: Text.T;
                        VAR candidateBBoard: BOOLEAN): FolderPtr;
  (* Get next candidate folder for searching *)

PROCEDURE CreateFolder (folder: Text.T) RAISES {Error};

PROCEDURE ChangeFolders () RAISES {};
  (* Wait for background operations to complete. *)

PROCEDURE RescanFolder (folder: Text.T) RAISES { Error };
  (* Deletes .inodecache file so it will be regenerated *)

PROCEDURE SortFolder (folder: Text.T) RAISES {Error};

PROCEDURE RemoveFolder (folder: Text.T): FolderPtr RAISES {Error};
(* Irreversibly delete the folder and its contents. Returns the FolderPtr
   corresponding to this folder, so that the client can ensure
   GetNextBBWithNews is never again called with this value. *)

PROCEDURE FolderExists (folder: Text.T): BOOLEAN;

PROCEDURE FolderList (): TextList.T;
  (* Returns list of private folders plus bulletin boards *)

PROCEDURE IsBBoard (folder: Text.T): BOOLEAN;
  (* TRUE iff folder is a bulletin board *)

PROCEDURE Incoming (folder: Text.T): BOOLEAN;
  (* TRUE iff folder receives new messages: bulletin boards and inbox *)


(********************)
(* Mailbox Routines *)
(********************)

PROCEDURE NewMailPoll(): INTEGER RAISES {Error};
  (* Polls mail spool for new messages, and return the count of them *)


(*****************************)
(* Bulletin Board Procedures *)
(*****************************)

PROCEDURE GetNextBBoard(b: FolderPtr; checkNewsDir: BOOLEAN;
                        VAR (*out*) folder: Text.T;
                        VAR (*out*) hasNews: BOOLEAN): FolderPtr RAISES {};
(* Return the first bboard "following" b, where:
     - the folder following NIL is the first folder
     - the folder following the last folder is NIL
   Returns in "hasNews" an indicator of whether found bboard has new news.
   To decide if a particular bboard has news, just checks an internal cache,
   unless checkNewsDir=TRUE, in which case actually examines the news spool. *)

PROCEDURE GetNewsCount (): CARDINAL;
  (* Return approximate count of bboard folders with new news. *)


(************************)
(* Folder Scan Procedures *)
(************************)

PROCEDURE GetMsgList(folder: Text.T; forceScan, withInc: BOOLEAN;
                     VAR curMsgId: TEXT): Rd.T
                     RAISES {Error};
(* Return reader for scan file for given folder, first incorporating new mail
   or news.  If forceScan is TRUE, the scan file will be regenerated regardless
   of whether it appears to be necessary.  The scan file is one line per
   message. Each line is some junk followed by ":", then the "header".  The
   "header" is an "id" followed by whitespace followed by good stuff like
   date, sender and subject.  The "id" is passed back to various message
   access procedures in this interface.  Caller must close the reader.
   Iff "withInc", do an inc before scanning.  "curMsgId" returns the msgID
   most recently set by "SetCurrent", or recorded in the file system by
   some other mail handler. *)

PROCEDURE SetCurrent(folder, msgID: TEXT) RAISES {};
  (* Records this msgID as "current" for this folder; suppresses errors.
     For inbox and bboards, records in the file system immediately; otherwise
     records sometime later in a background thread. *)

PROCEDURE GetInodecache(folder: Text.T): Rd.T RAISES {Error};
(* Returns reader for scan file for given folder, without any incorporaton of
   news or mail.  Format as for GetMsgList.  Caller must close the reader. *)

PROCEDURE PurgeFolder(folder: Text.T; lastToDelete: TEXT) RAISES {Error};
(* removes messages [1..lastToDelete], and repacks folder; "lastToDelete" is
   a message "id", as defined in GetMsgList. *)

PROCEDURE BackgroundScanning (on: BOOLEAN);
(* whether to do backgrounnd inc and scanning *)


(*****************)
(* NI procedures *)
(*****************)

TYPE NI = OBJECT
    rd: Rd.T;
    wr: Wr.T;
  METHODS
    getCount(): INTEGER RAISES { Error };
    kill();
  END;

PROCEDURE StartNI(): NI RAISES {Error};
  (* Start an NI sub-process *)


(******************)
(* Message Access *)
(******************)

PROCEDURE MsgPath(folder, msgID: Text.T): Text.T;
  (* Return full path name for the message *)

PROCEDURE GetMsg(folder, msgID: Text.T;
                 VAR (*OUT*) header, brief, body: TEXT) RAISES {Error};
  (* Returns contents of the speciified message, broken out into:
       "header" - the entire original header
       "brief"  - only the interesting fields of the header
       "body"   - everything after the original header
     *)

PROCEDURE CopyMsg(srcFolder, toFolder: Text.T; msgList: TextList.T) RAISES {Error};

PROCEDURE DeleteMsg (folder: Text.T; msgList: TextList.T) RAISES {Error};

PROCEDURE MoveMsg(srcFolder, toFolder: Text.T; msgList: TextList.T) RAISES {Error};

PROCEDURE PrintMsg(folder: Text.T; msgList: TextList.T; printFilter: Text.T)
                   RAISES {Error};

PROCEDURE SaveMsg(folder: Text.T; msgList: TextList.T; toFileName: Text.T)
                  RAISES {Error};

PROCEDURE MsgSubject(folder, msgID: Text.T): Text.T RAISES {Error};
  (* Returns message subject, or "" if there is none *)

PROCEDURE ReplyMsg(folder, msgID: Text.T;
                   replyToSender, ccToMe: BOOLEAN;
                   includeMessageInDraft: BOOLEAN;
                   includeReplyString: Text.T;
                   VAR (*out*) to, cc, subject, inReplyTo, body: Text.T)
             RAISES {Error};

PROCEDURE ForwardMsg
  (folder: Text.T; msgList: TextList.T; ccToMe: BOOLEAN; VAR
   (*out*) cc, body: Text.T) RAISES {Error};

PROCEDURE IncludeMsg (folder, msgList, prefix: Text.T): Text.T
   RAISES {Error};
  (* Returns message, with the prefix string starting each line. *)

PROCEDURE ReadFileText(fileName: TEXT): TEXT RAISES {Error};
  (* Returns the entire contents of the file, intact. *)


(***********)
(* Sending *)
(***********)

PROCEDURE DraftFileName(draftFile: CARDINAL; prefix: TEXT := ""): TEXT;
  (* Returns the full path name of the file written by WriteDraftFile.
     "prefix" is for internal use only. *)

PROCEDURE GetOldDrafts(): REF ARRAY OF CARDINAL RAISES { Error };
  (* Returns a list of existing draft files; raises Error if the
     draft directory might exist but can't be enumerated. *)

PROCEDURE DraftFileTime(draftFile: CARDINAL): Time.T RAISES { Error };
  (* Returns the mtime of the draft file *)

PROCEDURE ReadDraftFile(draftFile: CARDINAL): TEXT RAISES { Error };
  (* Returns the contents of the given draft file *)

PROCEDURE WriteDraftFile(msg: TEXT; draftFile: CARDINAL) RAISES {Error};
  (* Writes msg into a place suitable for message drafts, creating if needed *)

PROCEDURE DiscardDraftFile(draftFile: CARDINAL) RAISES { Error };
  (* Ensures that draft file will not be returned by future GetOldDrafts *)

PROCEDURE SendMsg(draftFile: CARDINAL) RAISES {Error};
  (*Sends the message contained in given draft file. *)
  (* Note that a successful send renames the file; see "man post" *)

(******************)
(* Initialization *)
(******************)

TYPE
  ErrorHandler = Closure.T;

PROCEDURE BeginInit (user, home: Text.T; handler: ErrorHandler);
(* Call this to begin the initialization process, some of which can be
   executed concurrently.  If a background operation encounters an error,
   proc will be called with arg. *)

PROCEDURE CompleteInit ();
(* Call this to complete the initialization process; only when CompleteInit
   returns may other UnixMail procedures be called. *)


(***********)
(* Cleanup *)
(***********)

PROCEDURE CleanUp (); (* Must be called before terminating execution. *)


END UnixMail.
