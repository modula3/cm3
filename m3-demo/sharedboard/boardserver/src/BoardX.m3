(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* This program allows the user to type and delete text items on a
   whiteboard. *)

MODULE BoardX;

IMPORT Thread, NetObj, SmallDB, Pickle, OSError,
       Item, ItemList, ItemTbl, AtomicItemTbl, 
       TextItem, RuleItem, <* NOWARN *> (* must be included *)
       Callback, RectR, Log,
       ClientInfo, ClientInfoX, ClientInfoList, AtomicClientList, 
       NotifyRec, NotifyQueue;

REVEAL T = Public BRANDED OBJECT
    items: AtomicItemTbl.T;
    clients: AtomicClientList.T;
    nq: NotifyQueue.T;
    stable: SmallDB.T;
  OVERRIDES
    init := Init;
    register := Register;
    unregister := Unregister;
    setScope := SetScope;
    createItems := CreateItems;
    modifyItems := ModifyItems;
    deleteItems := DeleteItems;
  END;

CONST ExpectedItems = 1000;

PROCEDURE Init (bd: T; stable: SmallDB.T; recover: BOOLEAN): T 
    RAISES {OSError.E, SmallDB.CorruptedDB, Pickle.Error} =
  VAR nc := NEW (NotifyClosure, bd := bd);
      state: AtomicItemTbl.State;
  BEGIN 
    IF recover THEN
      state := stable.recoverSnapshot ();
      Log.Recover (stable, state);
    ELSE 
      state := NEW (AtomicItemTbl.State, 
                    tbl := NEW (ItemTbl.Default).init (ExpectedItems),
                    id := 0);
      stable.snapshot (state);
    END;
    bd.items := NEW (AtomicItemTbl.T, state := state);
    bd.clients := NEW (AtomicClientList.T);
    bd.nq := NEW (NotifyQueue.T).init ();
    bd.stable := stable;
    EVAL Thread.Fork (nc);
    RETURN bd;
  END Init; 

PROCEDURE Register (bd: T; cb: Callback.T): ClientInfo.T =
  VAR ci := NEW (ClientInfoX.T).init (cb);
  BEGIN
    bd.clients.add (ci);
    RETURN ci;
  END Register;

PROCEDURE Unregister (bd: T; ci: ClientInfo.T) =
  BEGIN
    bd.clients.remove (ci);
  END Unregister;

PROCEDURE SetScope (bd: T; ci: ClientInfo.T; scope: RectR.T) =
  VAR nr := NEW (NotifyRec.T, code := NotifyRec.Code.Scope, doer := ci, 
                 newScope := scope);
  BEGIN
    bd.nq.enq (nr);
  END SetScope;

PROCEDURE CreateItems (bd: T; ci: ClientInfo.T; 
                       its: Item.TArray): Item.IDArray =
  VAR nr := NEW (NotifyRec.T, code := NotifyRec.Code.Create, doer := ci,
                 its := its);
      ids: Item.IDArray;
  BEGIN
    IF its = NIL THEN RETURN NIL END;
    ids := NEW (Item.IDArray, NUMBER (its^));
    LOCK bd.items DO
      FOR i := FIRST (its^) TO LAST (its^) DO
        INC (bd.items.state.id);
        ids[i] := bd.items.state.id;
        its[i].id := ids[i];
        EVAL bd.items.state.tbl.put (its[i].id, its[i]);
      END;
    END;
    bd.nq.enq (nr);
    RETURN ids;
  END CreateItems;
    
PROCEDURE ModifyItems (bd: T; ci: ClientInfo.T; 
                       its: Item.TArray; additive: BOOLEAN) =
  VAR nr := NEW (NotifyRec.T, code := NotifyRec.Code.Modify, doer := ci, 
                 its := its, additive := additive);
    old: Item.T;
  BEGIN
    IF its = NIL THEN RETURN END;
    LOCK bd.items DO
      FOR i := FIRST (its^) TO LAST (its^) DO
        IF its[i] = NIL OR NOT bd.items.state.tbl.get (its[i].id, old) THEN
          its[i] := NIL;
        ELSE
          EVAL bd.items.state.tbl.put (its[i].id, its[i]);
        END;
      END;
    END;
    bd.nq.enq (nr);
  END ModifyItems; 
    
PROCEDURE DeleteItems (bd: T; ci: ClientInfo.T; ids: Item.IDArray) =
  VAR nr := NEW (NotifyRec.T, code := NotifyRec.Code.Delete, doer := ci, 
                 ids := ids);
    old: Item.T;
  BEGIN
    IF ids = NIL THEN RETURN END;
    LOCK bd.items DO
      FOR i := FIRST (ids^) TO LAST (ids^) DO
        EVAL bd.items.state.tbl.delete (ids[i], old);
      END;
    END;
    bd.nq.enq (nr);
  END DeleteItems; 

TYPE NotifyClosure = Thread.Closure OBJECT
    bd: T;
    OVERRIDES 
      apply := NotifyLoop;
    END;
  
PROCEDURE NotifyLoop (nc: NotifyClosure): REFANY =
  VAR nr: NotifyRec.T;
  BEGIN
    LOOP
      nr := nc.bd.nq.deq ();
      Notify (nc.bd, nr);
    END;
  END NotifyLoop;

PROCEDURE Notify (bd: T; nr: NotifyRec.T) =
  BEGIN
      CASE nr.code OF

        NotifyRec.Code.Scope =>
        LOCK bd.items DO
          VAR ir := bd.items.state.tbl.iterate ();
              id: Item.ID;
              it: Item.T;
              il: ItemList.T := NIL;
              oldScope := nr.doer.getScope(); <*NOWARN*>
          BEGIN
            WHILE ir.next (id, it) DO
              IF RectR.Overlap (it.box, nr.newScope)
                (* AND NOT RectR.Overlap (it.box, oldScope) *) 
               THEN
                il := ItemList.Cons (it, il);
              END;
            END;
            VAR its := NEW (Item.TArray, ItemList.Length (il));
                i := 0;
            BEGIN
              WHILE il # NIL DO
                its [i] := il.head;
                il := il.tail;
                INC (i);
              END;
              TRY
                nr.doer.getCallback().itemsCreated (its);
                nr.doer.setScope (nr.newScope);
              EXCEPT 
                NetObj.Error, Thread.Alerted => Unregister (bd, nr.doer);
              END;
            END;
          END;
        END;

      | NotifyRec.Code.Create =>
        VAR deadClients: ClientInfoList.T := NIL; BEGIN
          LOCK bd.clients DO
            VAR clients := bd.clients.list; BEGIN
              WHILE clients # NIL DO
                IF clients.head # nr.doer THEN
                  TRY
                    clients.head.getCallback().itemsCreated (nr.its);
                  EXCEPT 
                    NetObj.Error, Thread.Alerted => 
                    deadClients := ClientInfoList.Cons (clients.head, 
                                                        deadClients);
                  END;
                END;
                clients := clients.tail;
              END;
            END;
          END;
          (* must remove deadClients after unlocking bd.clients *)
          WHILE deadClients # NIL DO
            Unregister (bd, deadClients.head);
            deadClients := deadClients.tail;
          END;
        END;
        TRY
          Log.Update (bd.stable, bd, nr);
        EXCEPT
          OSError.E => (* ideally, should inform client *)
        END;

      | NotifyRec.Code.Modify =>
        VAR deadClients: ClientInfoList.T := NIL; BEGIN
          LOCK bd.clients DO
            VAR clients := bd.clients.list; BEGIN
              WHILE clients # NIL DO
                IF clients.head # nr.doer THEN
                  TRY
                    clients.head.getCallback().itemsModified (nr.its, 
                                                              nr.additive);
                  EXCEPT 
                    NetObj.Error, Thread.Alerted => 
                    deadClients := ClientInfoList.Cons (clients.head, 
                                                        deadClients);
                  END;
                END;
                clients := clients.tail;
              END;
            END;
          END;
          (* must remove deadClients after unlocking bd.clients *)
          WHILE deadClients # NIL DO
            Unregister (bd, deadClients.head);
            deadClients := deadClients.tail;
          END;
        END;
        TRY
          Log.Update (bd.stable, bd, nr);
        EXCEPT
          OSError.E => (* ideally, should inform client *)
        END;

      | NotifyRec.Code.Delete =>
        VAR deadClients: ClientInfoList.T := NIL; BEGIN
          LOCK bd.clients DO
            VAR clients := bd.clients.list;  BEGIN
              WHILE clients # NIL DO
                IF clients.head # nr.doer THEN
                  TRY
                    clients.head.getCallback().itemsDeleted (nr.ids);
                  EXCEPT 
                    NetObj.Error, Thread.Alerted => 
                    deadClients := ClientInfoList.Cons (clients.head, 
                                                        deadClients);
                  END;
                END;
                clients := clients.tail;
              END;
            END;
          END;
          (* must remove deadClients after unlocking bd.clients *)
          WHILE deadClients # NIL DO
            Unregister (bd, deadClients.head);
            deadClients := deadClients.tail;
          END;
        END;
        TRY
          Log.Update (bd.stable, bd, nr);
        EXCEPT
          OSError.E => (* ideally, should inform client *)
        END;

      END;
  END Notify;

PROCEDURE Busy (bd: T): BOOLEAN =
  BEGIN
    RETURN bd.clients.list # NIL;
  END Busy;

PROCEDURE Save (board: T) RAISES {OSError.E} =
  BEGIN
     LOCK board.items DO
       board.stable.snapshot (board.items.state);
     END;
  END Save;


PROCEDURE Quit (<*UNUSED*> board: T) =
  BEGIN
    (* Not implemented: Should stop notify thread. *)
  END Quit;


BEGIN
END BoardX.

