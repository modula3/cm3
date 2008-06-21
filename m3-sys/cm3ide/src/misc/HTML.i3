
INTERFACE HTML;

IMPORT Wr, Thread;
IMPORT ID, Node, Wx;

PROCEDURE PutImg (name: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};
PROCEDURE PutSmallImg (name: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};

PROCEDURE ImgRef (name: TEXT): TEXT;
(* return the HTML reference to the icon named "name". *)

PROCEDURE ViewOnly (act: ID.T;  data: Node.FormData;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};
(* Generate a warning in the HTML on "wx" if "act" is not "view"
   or if any form data was passed. *)

PROCEDURE NoAction (act: ID.T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};
(* Generate a warning in the HTML on "wx" claiming "act" is not supported. *)

PROCEDURE NoData (data: Node.FormData;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};
(* If "data" is non-"NIL", generate a warning in the HTML on "wx". *)

PROCEDURE MakeURL (a, b, c, d: TEXT := NIL): TEXT;
(* Concatenates the non-NIL parameters with internal slashes. *)

PROCEDURE Begin (n: Node.T;  wx: Wx.T)  RAISES {Wr.Failure, Thread.Alerted};
PROCEDURE End (wx: Wx.T)  RAISES {Wr.Failure, Thread.Alerted};
(* Generate the HTML goo around the outside. *)
(* Note that 'End' first calls 'GenCopyright' as required by Farshad Nayeri for open-source release *)

PROCEDURE BeginXX (n: Node.T;  wx: Wx.T;  t1, t2, t3, icon: TEXT := NIL)
  RAISES {Wr.Failure, Thread.Alerted};
(* Like "Begin", but uses title "t1 & t2 & t3" *)

PROCEDURE BeginYY (n: Node.T;  wx: Wx.T;  t1, t2, t3: TEXT := NIL)
  RAISES {Wr.Failure, Thread.Alerted};
(* Like "BeginXX", but doesn't generate a title in the HTML body. *)

PROCEDURE GenURL (n: Node.T;  wx: Wx.T)  RAISES {Wr.Failure, Thread.Alerted};
(* Write "n"'s URL on "wx" *)

PROCEDURE GenPathFinder (n: Node.T;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};
(* Write the list of header buttons that leads to node "n" on "wx". *)

(***
PROCEDURE GenLocation (n: Node.T;  wx: Wx.T) RAISES {Wr.Failure, Thread.Alerted};
***)
(* Write an HTTP "Location:" header to node "n" on "wx". *)

PROCEDURE GenBase (n: Node.T;  wx: Wx.T;  leaf := FALSE)
  RAISES {Wr.Failure, Thread.Alerted};
(* Write an HTML base reference to node "n" on "wx". *)

PROCEDURE GenFileRef (path: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};
(* Write an HTML reference to file "path" on "wx". *)

PROCEDURE GenRef (n: Node.T;  wx: Wx.T;  tag: TEXT := NIL)
  RAISES {Wr.Failure, Thread.Alerted};
(* Write an HTML reference to node "n" on "wx". *)

PROCEDURE GenActionRef (n: Node.T;  wx: Wx.T;  act: TEXT;  tag: TEXT := NIL)
  RAISES {Wr.Failure, Thread.Alerted};
(* Write an HTML reference on "wx" that will to perform the
   "act" opertaion on node "n". *)

PROCEDURE GenChoices (VAR results: Node.Set;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};
(* Produce a directory of choices for "results" on "wx". *)

PROCEDURE NodeURL (n: Node.T): TEXT;
(* Returns the fully qualified URL for node "n". *)

PROCEDURE GenCopyright (wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted};
(* Write the copyright legend on "wx".  
For the open-source release, Farshad Nayeri requires that this legend appear on all pages served by CM3-IDE. *)

END HTML.
