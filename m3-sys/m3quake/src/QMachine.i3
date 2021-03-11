(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Feb 20 13:33:35 PST 1995 by kalsow     *)

INTERFACE QMachine;

IMPORT Thread, Wr, QValue, QCode;
FROM Quake IMPORT Machine, Error, ID, IDMap;
IMPORT QPromiseSeq, ETimer;

REVEAL
  T <: T_;
TYPE
  T = Machine;
  T_ = OBJECT
    map: IDMap := NIL; (* READONLY *)
    promises : QPromiseSeq.T;
    timer : ETimer.T;
  METHODS
    init      (map: IDMap): T;
    evaluate  (s: QCode.Stream)                     RAISES {Error, Thread.Alerted};
    put       (nm: ID;  READONLY val: QValue.T)                RAISES {Error};
    get       (nm: ID;  VAR(*OUT*) val: QValue.T): BOOLEAN;
    lookup    (nm: ID): QValue.Binding;
    push      (READONLY val: QValue.T);
    pop       (VAR(*OUT*) val: QValue.T)                       RAISES {Error};
    error     (msg: TEXT)                                      RAISES {Error};
    cleanup   ()                                               RAISES {Error};
    include   (file: TEXT)                          RAISES {Error, Thread.Alerted};
    normalize (prefix, path: TEXT): TEXT                       RAISES {Error};
    start_call(READONLY proc: QValue.T)                        RAISES {Error};
    call_proc (n_args: INTEGER;  isFunc: BOOLEAN)   RAISES {Error, Thread.Alerted};
    cp_if     (src, dest: TEXT)                                RAISES {Error};
    make_dir  (dir: TEXT)                                      RAISES {Error};
    cur_file  (): TEXT;
    cur_path  (): TEXT;
    cur_wr    (): Wr.T;
    set_wr    (wr: Wr.T);
    exec_echo (b: BOOLEAN): BOOLEAN;
    trace     (b: BOOLEAN);
    
    record(on : BOOLEAN);     (* instead of performing certain acts, promise *)
  END;

PROCEDURE PushBool (t: T;  b: BOOLEAN);
PROCEDURE PushText (t: T;  s: TEXT);
PROCEDURE PushInt  (t: T;  i: INTEGER);
PROCEDURE PushID   (t: T;  x: ID);

PROCEDURE PopBool (t: T): BOOLEAN    RAISES {Error};
PROCEDURE PopText (t: T): TEXT       RAISES {Error};
PROCEDURE PopInt  (t: T): INTEGER    RAISES {Error};
PROCEDURE PopID   (t: T): ID         RAISES {Error};

PROCEDURE GetEnv (default, v0, v1, v2, v3, v4: TEXT := NIL): TEXT;

END QMachine.



