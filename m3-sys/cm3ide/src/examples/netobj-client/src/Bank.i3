
INTERFACE Bank;
IMPORT NetObj;
FROM NetObj IMPORT Error;
FROM Thread IMPORT Alerted;

TYPE
  T = NetObj.T OBJECT METHODS
    findAccount (acct: AcctNum): Account RAISES {Alerted, Error};
  END;

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

