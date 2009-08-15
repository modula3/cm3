
INTERFACE Bank;
IMPORT NetObj;
(* "NetObj" is the primary interface for building network object
   applications. *)

FROM NetObj IMPORT Error;
FROM Thread IMPORT Alerted;
(* "NetObj.Error" and "Thread.Alerted" may be raised by network object
    operations. *)

(* Type "Bank.T" is a network object which supports the operation
   "findAccount", which returns a "Bank.Account" object. *)

TYPE
  T = NetObj.T OBJECT METHODS
    findAccount (acct: AcctNum): Account RAISES {Alerted, Error};
  END;

(* Type "Bank.Account" supports operations "deposit", "withdraw" 
   and "get_balance". Network object operations can raise user-defined
   exceptions such as "BadAmount", and "InsufficientFunds". *)
   
TYPE
  Account = NetObj.T OBJECT METHODS
    deposit (amount: REAL) RAISES {BadAmount, Alerted, Error};
    withdraw (amount: REAL) RAISES {BadAmount, InsufficientFunds, Alerted, Error};
    get_balance (): REAL RAISES {Alerted, Error};
  END;

TYPE
  AcctNum = [1..100];

EXCEPTION
  BadAmount;
  InsufficientFunds;

END Bank.

