
MODULE NetObjClient EXPORTS Main;
IMPORT Bank, NetObj, Thread;
IMPORT IO, Fmt;

CONST
  MyBank : TEXT = "LastNationalBank";
  MyAcct : Bank.AcctNum = 99;

VAR
  bank: Bank.T;
  acct: Bank.Account;
BEGIN
  TRY
    bank := NetObj.Import (MyBank);
    acct := bank.findAccount(MyAcct);
    WITH balance = acct.get_balance() DO
      IO.Put ("My account balance is " & Fmt.Real(balance) &"\n");
    END;
  EXCEPT
  | NetObj.Error => IO.Put ("A network error occured\n");
  | Bank.BadAmount => <* ASSERT FALSE *>
  (* BadAmount will not be raised for positive deposits. *)
  END;
END NetObjClient.
