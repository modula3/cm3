(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE AtomicClientList;

IMPORT ClientInfo, ClientInfoList;

REVEAL T = Public BRANDED "AtomicClientList" OBJECT
     OVERRIDES
       add := Add;
       remove := Remove;
     END;

PROCEDURE Add (t: T; ci: ClientInfo.T) =
  BEGIN
    LOCK t DO
      t.list := ClientInfoList.Cons (ci, t.list);
    END;
  END Add;

PROCEDURE Remove (t: T; ci: ClientInfo.T) =
  BEGIN
    LOCK t DO
      IF NOT ClientInfoList.Member (t.list, ci) THEN RETURN END;
      IF t.list.head = ci THEN 
        t.list := t.list.tail;
      ELSE
        VAR list := t.list; BEGIN
          WHILE list.tail.head # ci DO
            list := list.tail;
          END;
          list.tail := list.tail.tail;
        END;
      END;
    END;
  END Remove;

BEGIN
END AtomicClientList.
