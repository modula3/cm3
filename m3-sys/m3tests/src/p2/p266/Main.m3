
(* Test to expose a bug, called the "Twice used ticket" bug, 
   in ThreadWin32.m3:  Multiple waiters take multiple tickets 
   when only one was provided by Signal.  The result is that
   the tickets count of a Condition variable goes negative, 
   causing later lost wakeups in Signal.
*) 

MODULE Main 

; IMPORT Fmt 
; IMPORT Stdio 
; IMPORT Thread 
; IMPORT Wr

; PROCEDURE W ( Msg : TEXT ) 

  = <* FATAL Thread . Alerted *>
    <* FATAL Wr . Failure *>
    BEGIN
      Wr . PutText ( Stdio . stdout , Msg ) 
    ; Wr . PutText ( Stdio . stdout , Wr . EOL ) 
    ; Wr . Flush ( Stdio . stdout ) 
    END W 

; TYPE ThreadNo = [ 0 .. 4 ] 
; CONST ThreadNoNull = 0

; PROCEDURE ThImage ( ThN : ThreadNo ) : TEXT 
  = BEGIN 
      RETURN "Thread " & Fmt . Int ( ThN ) 
    END ThImage

(* We want to deterministically control the sequence of thread operations,
   to create a test case.  It is hard to do this without using Condition
   variables, but we are testing Condition variables, so can't really
   assume they work correctly.  We resort to using a hodge-podge of 
   MUTEXs, busy waiting, and pauses, hoping they are long enough. 
*) 

; CONST BusyWaitInterval = 0.1D0 
; CONST QuiesceInterval = 3.0D0
 
; TYPE State = { Null , Idle , Acq , Rel , Wait , Wait2 , Sig } 
; TYPE StateSet = SET OF State 
; VAR StateMutex : MUTEX 
; VAR States := ARRAY ThreadNo OF State { State . Null , .. } 
      (* LL = StateMutex *)
; VAR Holder : ThreadNo := ThreadNoNull  
      (* LL = StateMutex *)

; PROCEDURE StateImage ( St : State ) : TEXT 

  = VAR LResult : TEXT 

  ; BEGIN 
      CASE St 
      OF State . Null => LResult := "null"
      | State . Idle => LResult := "idle"
      | State . Acq => LResult := "entering Acquire"
      | State . Rel => LResult := "entering Release"
      | State . Wait => LResult := "entering Wait"
      | State . Wait2 => LResult := "asleep in Wait"
      | State . Sig => LResult := "entering Signal"
      END (* CASE *)
    ; RETURN LResult 
    END StateImage 

; EXCEPTION Failure 

; VAR ActionMutex : MUTEX 
; TYPE ActionProc = PROCEDURE ( ThN : ThreadNo ) 
; VAR ActionProcs := ARRAY ThreadNo OF ActionProc { NIL , .. }
      (* LL = ActionProc *)  

; PROCEDURE Action ( ThN : ThreadNo ; Apply : ActionProc ) 
  (* Ask thread number ThN to execute Apply. *) 

  = BEGIN 
      LOCK ActionMutex 
      DO WITH WAct = ActionProcs [ ThN ] 
        DO 
          <* ASSERT WAct = NIL *> 
          WAct := Apply
        END
      END
    ; LOOP (* Wait for test thread to take the direction. *) 
        Thread . Pause ( BusyWaitInterval ) 
      ; LOCK ActionMutex 
        DO IF ActionProcs [ ThN ] = NIL THEN EXIT END (* IF *) 
        END (* LOCK *)
      END (* LOOP *) 
    END Action 

; PROCEDURE ActionWait ( ThN : ThreadNo ; Apply : ActionProc ) 
  RAISES { Failure } 
  (* Ask thread number  ThN to execute Apply, expecting Apply to do a 
     Wait(TestMutex, TestCondition), which further implies 
     ThN already holds TestMutex.  Note that it waited by its release
     of TextMutex. *) 

  = BEGIN 
      Action ( ThN , Apply ) 
    ; LOOP (* Wait for ThN to release TestMutex.  No other thread is
              contending for TestMutex, so this is an indication that 
              ThN has stopped inside Wait. *) 
        LOCK TestMutex 
        DO LOCK StateMutex 
          DO WITH WSt = States [ ThN ] 
            DO IF WSt # State . Wait 
              THEN
                W ( ThImage ( ThN ) & " Failed to wait in Wait." )
              ; RAISE Failure  
              ELSE 
                <* ASSERT Holder = ThN *> 
                Holder := ThreadNoNull
              ; WSt := State . Wait2 
              ; W ( ThImage ( ThN ) & " Waiting." )
              ; EXIT
              END (* IF *) 
            END (* WITH *) 
          END (* LOCK StateMutex *) 
        END (* LOCK TestMutex*) 
      END (* LOOP *) 
    END ActionWait  

; PROCEDURE WaitForHeld ( ) : ThreadNo 
  (* Busy wait until some thread holds TestMutex, then return its
     ThreadNo. *) 
  = VAR LHolder : ThreadNo 

  ; BEGIN
      LOOP  
        Thread . Pause ( BusyWaitInterval ) 
      ; LOCK StateMutex DO LHolder := Holder END (* LOCK *) 
      ; IF LHolder # ThreadNoNull THEN EXIT END 
      END (* LOOP *) 
    ; RETURN LHolder 
    END WaitForHeld

; PROCEDURE WaitForStateSet ( ThN : ThreadNo ; Sts : StateSet ) 
  (* Busy wait for thread number ThN to get into one of Sts. *) 
  = BEGIN 
      LOOP 
        LOCK StateMutex 
        DO WITH WSt = States [ ThN ]
          DO IF WSt IN Sts 
            THEN (* This is what we want. *) 
              EXIT 
            END (* IF *) 
          END (* WITH *) 
        END (* LOCK *)
      ; Thread . Pause ( BusyWaitInterval ) 
      END (* LOOP *) 
    END WaitForStateSet 

; PROCEDURE WaitForState ( ThN : ThreadNo ; St : State ) 
  (* Busy wait for action thread to get into St. *) 
  = BEGIN 
      WaitForStateSet ( ThN , StateSet { St } ) 
    END WaitForState 

; PROCEDURE NoteWhetherStateSet 
    ( ThN : ThreadNo ; Sts : StateSet ; YesMsg , NoMsg := "" ) : BOOLEAN 
  (* Write whether thread number ThN is in one of Sts and return it. *)

  = VAR LResult : BOOLEAN 
  ; VAR LState : State 
  ; VAR LMsg : TEXT 

  ; BEGIN 
      LOCK StateMutex 
      DO
        LState := States [ ThN ]
      ; LResult := LState IN Sts 
      ; LMsg := ThImage ( ThN ) & " is " & StateImage ( LState ) 
      ; IF LResult 
        THEN IF YesMsg # NIL THEN LMsg := LMsg & YesMsg END 
        ELSE IF NoMsg # NIL THEN LMsg := LMsg & NoMsg END
        END (* IF *) 
      ; W ( LMsg ) 
      END (* LOCK *) 
    ; RETURN LResult
    END NoteWhetherStateSet

; PROCEDURE NoteWhetherState 
    ( ThN : ThreadNo ; St : State ; YesMsg , NoMsg := "" ) : BOOLEAN 
  (* Write whether thread number ThN is in St and return it. *)
  = BEGIN 
      RETURN NoteWhetherStateSet ( ThN , StateSet { St } , YesMsg , NoMsg ) 
    END NoteWhetherState

; VAR TestMutex : MUTEX 
; VAR TestCond : Thread . Condition

; VAR Threads : ARRAY ThreadNo OF Thread . T 

; TYPE Cl = Thread . Closure 
        OBJECT 
          ClThN : ThreadNo 
        OVERRIDES apply := TestApply 
        END 
; VAR Closures : ARRAY ThreadNo OF Cl  

; PROCEDURE TestApply ( Self : Cl ) : REFANY 
  (* Loop for threads that execute test actions on Mutexs and Conditions. *) 
  = VAR LProc : ActionProc 
  ; BEGIN 
      LOOP (* Busy wait for work. *) 
        Thread . Pause 
          ( 0.2D0 (* Not too extremely busy. *) 
            + FLOAT ( Self . ClThN , LONGREAL ) * 0.01D0 
              (* ^Avoid staying in phase. *)
          ) 
      ; LOCK ActionMutex 
        DO WITH WProc = ActionProcs [ Self . ClThN ] 
          DO IF WProc # NIL 
             THEN (* We have some work to do. *) 
               LProc := WProc 
             ; WProc := NIL 
             END (* IF *) 
          END (* WITH *) 
        END (* LOCK *) 
      ; IF LProc # NIL 
        THEN LProc ( Self . ClThN )
             (* ^May or may not return right away. *) 
        ; LProc := NIL 
        END (* IF *) 
      END (* LOOP *) 
    ; <* NOWARN *> (* Intentionally unreachable. *) RETURN NIL 
    END TestApply 

; PROCEDURE DoAcq ( ThN : ThreadNo ) 
  = BEGIN
      LOCK StateMutex DO States [ ThN ] := State . Acq END 
    ; W ( ThImage ( ThN ) & " Entering Acquire of TestMutex." ) 
    ; Thread . Acquire ( TestMutex ) 
    ; W ( ThImage ( ThN ) & " Acquired TestMutex." ) 
    ; LOCK StateMutex 
      DO WITH WThN = States [ ThN ]
        DO 
          <* ASSERT WThN = State . Acq *>  
          WThN := State . Idle  
        ; <* ASSERT Holder = ThreadNoNull *>
          Holder := ThN
        END (* WITH *) 
      END (* LOCK *) 
    END DoAcq

; PROCEDURE DoRel ( ThN : ThreadNo ) 
  = BEGIN 
      LOCK StateMutex DO States [ ThN ] := State . Rel END 
    ; W ( ThImage ( ThN ) & " Entering release of TestMutex." ) 
    ; Thread . Release ( TestMutex ) 
    ; W ( ThImage ( ThN ) & " Released TestMutex." ) 
    ; LOCK StateMutex 
      DO WITH WThN = States [ ThN ]
        DO 
          <* ASSERT WThN = State . Rel *>  
          WThN := State . Idle  
        ; <* ASSERT Holder = ThN *>
          Holder := ThreadNoNull 
        END (* WITH *) 
      END (* LOCK *) 
    END DoRel

; PROCEDURE DoWait ( ThN : ThreadNo ) 
  = BEGIN
      LOCK StateMutex 
      DO WITH WThN = States [ ThN ]
        DO 
          <* ASSERT Holder = ThN *>
          <* ASSERT WThN = State . Idle *>  
          WThN := State . Wait  
        END (* WITH *) 
      END (* LOCK *) 
    ; W ( ThImage ( ThN ) & " Entering Wait on TestCond." ) 
    ; Thread . Wait ( TestMutex , TestCond ) 
    ; W ( ThImage ( ThN ) 
          & " Was Signalled in TestCond and reacquired TestMutex." 
        ) 
    ; LOCK StateMutex 
      DO WITH WThN = States [ ThN ] 
        DO
          <* ASSERT WThN = State . Wait2 *> 
          States [ ThN ] := State . Idle 
        ; Holder := ThN 
        END (* WITH *) 
      END (* LOCK *) 
    END DoWait

; PROCEDURE DoSignal ( ThN : ThreadNo ) 
  = BEGIN
      LOCK StateMutex DO States [ ThN ] := State . Sig  END 
    ; W ( ThImage ( ThN ) & " Entering Signal on TestCond" ) 
    ; Thread . Signal ( TestCond ) 
    ; W ( ThImage ( ThN ) & " TestCond Signalled " ) 
    ; LOCK StateMutex DO States [ ThN ] := State . Idle END 
    END DoSignal

; PROCEDURE ForceSignalled ( ThN : ThreadNo ) 
  (* PRE: Only ThN could hold TestMutex.
     If thread ThN is asleep in TestCondition waiting for a Signal,
     Signal it and let it reacquire TestMutex.  Either way, finally
     make it release TestMutex. *) 

  = VAR LThN : ThreadNo 
  ; BEGIN
      (* Allow ThN time to acquire TestMutex, if it has been signalled. *) 
      Thread . Pause ( QuiesceInterval ) 
    ; IF NoteWhetherState ( ThN , State . Wait2 ) 
      THEN 
        Action ( 3 , DoSignal ) 
      ; WaitForState ( 3 , State . Idle ) 
      ; LThN := WaitForHeld ( ) 
      ; <* ASSERT LThN = ThN *>
      END (* IF *) 
    ; Action ( ThN , DoRel )
    ; WaitForState ( ThN , State . Idle ) 
    END ForceSignalled 

; PROCEDURE TestSeq ( ) 

  = VAR LThNo : ThreadNo 
  ; BEGIN 
      TRY 

      (* Thread 1 waits: *) 
        Action ( 1 , DoAcq ) 
      ; WaitForState ( 1 , State . Idle ) 
      ; ActionWait ( 1 , DoWait )
      ; WaitForState ( 1 , State . Wait2 ) 

      (* Thread 2 waits: *) 
      ; Action ( 2 , DoAcq ) 
      ; WaitForState ( 2 , State . Idle ) 
      ; ActionWait ( 2 , DoWait )
      ; WaitForState ( 2 , State . Wait2 ) 

      (* Here, there are two waiters. *) 
      (* The Wait implementation must unblock at least one of the waiting
         threads, but is allowed to unblock up to all of them.  The Posix
         and PThread implementations unblock only one.  The buggy Win32
         implementation unblocks both in unpredictable order.  We want to 
         get both unblocked from inside Wait and also let them acquire and
         release TestMutex, to set up consistently, in preparation for 
         testing for the actual bug. *) 

      (* Hold TestMutex to limit how far 1 and 2 can get after Wait unblocks
         them.  *) 
      ; Action ( 3 , DoAcq )
      ; WaitForState ( 3 , State . Idle ) 

      (* Signal. Each of 1 & 2, when unblocked, will block again trying to 
         reacquire TestMutex.*)
      ; Action ( 3 , DoSignal ) 
      ; WaitForState ( 3 , State . Idle ) 

      (* Now release TestMutex, so the first thread unblocked inside Wait 
         by the Signal will acquire it. *) 
      ; Action ( 3 , DoRel )
      ; WaitForState ( 3 , State . Idle ) 

      (* See what thread was unblocked. *) 
      ; LThNo := WaitForHeld ( ) 
      ; IF LThNo = 1 
        THEN (* 1 holds TestMutex. 2 will be asleep, either in 
                TestCondition or for TestMutex. *)
          Action ( 1 , DoRel )  
        ; WaitForState ( 1 , State . Idle )
        ; ForceSignalled ( 2 ) 
        ELSE (* 1 has not gotten out of Wait. 2 Must have done so. *) 
          <* ASSERT LThNo = 2 *>
          Action ( 2 , DoRel ) 
        ; WaitForState ( 2 , State . Idle )
        ; ForceSignalled ( 1 ) 
        END (* IF *) 

      (* Tread 4 Waits. *) 
      ; Action ( 4 , DoAcq ) 
      ; WaitForState ( 4 , State . Idle ) 
      ; ActionWait ( 4 , DoWait )
      ; WaitForState ( 4 , State . Wait2 ) 

      (* Thread 3 Signals. *) 
      ; Action ( 3 , DoAcq ) 
      ; WaitForState ( 3 , State . Idle ) 
      ; Action ( 3 , DoSignal ) 
      ; WaitForState ( 3 , State . Idle )
      ; Action ( 3 , DoRel ) 
      ; WaitForState ( 3 , State . Idle ) 

      (* Give 4 time to reacquire TestMutex, if it was signalled *) 
      ; Thread . Pause ( QuiesceInterval ) 

      (* See whether 4 got signalled. *) 
      ; WaitForStateSet ( 4 , StateSet { State . Wait2 , State . Idle } )  
      ; IF NoteWhetherState ( 4 , State . Idle ) 
        THEN 
          Action ( 4 , DoRel ) 
        ; WaitForState ( 4 , State . Idle ) 
        ; W ( "SUCCESS: all as expected.") 
        ELSE 
          W ( "FAILURE: This is the \"twice used ticket\" bug we are testing for." )
        ; RAISE Failure  
        END (* IF *) 
      EXCEPT
      | Failure => 
      END (* EXCEPT *) 
    END TestSeq

; PROCEDURE Init ( ) 
  = BEGIN
      ActionMutex := NEW ( MUTEX ) 
    ; StateMutex := NEW ( MUTEX ) 
    ; TestMutex := NEW ( MUTEX ) 
    ; TestCond := NEW ( Thread . Condition )
    ; FOR LThNo := FIRST ( ThreadNo ) TO LAST ( ThreadNo ) 
      DO
        Closures [ LThNo ] := NEW ( Cl , ClThN := LThNo )  
      ; States [ LThNo ] := State . Idle (* Waiting for directions. *) 
      ; Threads [ LThNo ] := Thread . Fork ( Closures [ LThNo ] )
      END (* FOR *) 
    END Init 

; BEGIN 
    Init ( ) 
  ; TestSeq ( ) 
  END Main 
.
