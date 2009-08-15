

MODULE NetObjServer EXPORTS Main;
IMPORT Bank, NetObj, Thread;
IMPORT IO, Fmt;

(* Create an implementation object for the "Bank.T" 
   network object. *)

TYPE
    BankImpl = Bank.T OBJECT
    accounts : ARRAY Bank.AcctNum OF Account;
  OVERRIDES
    findAccount := FindAccount;
  END;

(* Create an implementation object for "Account.T"
   network object. The "Account.T" uses a "MUTEX"
   to synchronize access to its balance. It also
   defines the operations "deposit", "withdraw", and "balance". *)

TYPE
  Account = Bank.Account OBJECT
    lock    : MUTEX;
    balance : REAL := 0.0;
  OVERRIDES
    deposit := Deposit;
    withdraw := Withdraw; (* not included *)
    get_balance := Balance; (* not included *)
  END;

PROCEDURE FindAccount (self: BankImpl; acct: Bank.AcctNum): Bank.Account =
(* Find an account in the table of accounts. *)
  BEGIN
    RETURN self.accounts[acct];
  END FindAccount;

PROCEDURE Deposit (self: Account; amount: REAL) RAISES {Bank.BadAmount} =
(* Deposit the money,
   making sure to serialize access with others trying to operate 
   on this account.*)
  BEGIN
    IF amount < 0.0 THEN RAISE Bank.BadAmount; END;
    LOCK self.lock DO
      self.balance := self.balance + amount;
    END;
  END Deposit;

PROCEDURE Withdraw (self: Account; amount: REAL) RAISES {Bank.BadAmount, Bank.InsufficientFunds} =
(* Withdraw the money,
   making sure to serialize access with others trying to operate 
  on this account.*)
  BEGIN
    IF amount < 0.0 THEN RAISE Bank.BadAmount; END;
    IF self.balance - amount < 0.0 THEN RAISE Bank.InsufficientFunds END;
    LOCK self.lock DO
      self.balance := self.balance - amount;
    END;
  END Withdraw;

PROCEDURE Balance (self: Account): REAL =
(* Get the balance, making sure to serialize access with 
      others trying to operate on this account.*)
  BEGIN
    LOCK self.lock DO
      RETURN self.balance;
    END;
  END Balance;

PROCEDURE NewBank () : BankImpl =
(* Creates a new bank by instantiating all the account objects. *)
  VAR b := NEW (BankImpl);
  BEGIN
    FOR i := FIRST (b.accounts) TO LAST (b.accounts) DO
      b.accounts[i] := NEW (Account, lock := NEW (MUTEX));
    END;
    RETURN b;
  END NewBank;

PROCEDURE PrintSummary() =
(* Prints a summary of all the active accounts, i.e., ones that have a positive 
   balance. *)
BEGIN  
  IO.Put (BankName & ": active account information\n");
  FOR i := FIRST(bank.accounts) TO LAST(bank.accounts) DO
    IF bank.accounts[i].balance > 0.0 THEN
     IO.Put (Fmt.Int(i) & ".......$" & Fmt.Real(bank.accounts[i].balance) & "\n");
    END;
  END;
END PrintSummary;

(* Name of bank to be exported. *)

CONST
  BankName = "LastNationalBank";

VAR
  bank := NewBank();

BEGIN

  IO.Put ("Starting bank server.\n");
  TRY 

    (* Export the bank obejct under "LastNationalBank". *)
    NetObj.Export (BankName, bank);
    IO.Put ("Bank server was exported as " & BankName & "\n");

    (* Print the summaries for accounts every 60 seconds. 
       Every incoming call to Network Objects will be serviced by a separate
       thread. *)
    LOOP 
      Thread.Pause (60.0D0);
      PrintSummary();
    END;
  EXCEPT (* If there is a problem, print an error and exit. *)
  |  NetObj.Error => IO.Put ("A network object failure occured.\n");
  |  Thread.Alerted => IO.Put ("Thread was alerted.\n");
  END;
 
END NetObjServer.


