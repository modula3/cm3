

MODULE NetObjClient EXPORTS Main;
(* An example of a Network Object client. *)

IMPORT Bank, NetObj, Thread;
IMPORT IO, Fmt, Scan, Text, FloatMode, Lex;

(* Global variables for this client. *)

VAR
  bank: Bank.T;
  acctnum: Bank.AcctNum;
  acct: Bank.Account := NIL;
  cmd: TEXT;

PROCEDURE Prompt(txt: TEXT): TEXT RAISES {IO.Error} =
(* Prints a prompt on the screen and asks for input from the user.
   If the current account is set, it will display the current
   account and the available balance. *)
  BEGIN
    TRY
      IF acct # NIL THEN
        IO.Put ("\n[acct:" & Fmt.Int(acctnum) & ", balance: $" & 
          Fmt.Real (acct.get_balance()) & "] ");
      END;
    EXCEPT
    ELSE 
    END; (* Since this is an informational message, we ignore all exceptions. *)
    IO.Put (txt & " : "); RETURN IO.GetLine();
  END Prompt;

EXCEPTION
  InvalidAccount;
  Quit;

(* Name used to import. *)

CONST
  BankName : TEXT = "LastNationalBank";

(* This client will take input commands and make calls to 
   network objects. As you can see, most of the work
   is in the reading of input from the user! *)

BEGIN
  TRY
    IO.Put ("Welcome to " & BankName & "\n");
    IO.Put ("Connecting to bank server...");
    bank := NetObj.Import (BankName);
    IO.Put ("done.\n");
    IO.Put ("Bank Teller Client Started...\n");
    IO.Put ("Valid commands are: \n" &
            "   account : set a current account for further transactions\n" &
            "   deposit : deposit into current account \n" & 
            "   withdraw: withdraw from the current account\n" & 
            "   balance : print balance for the current account\n" & 
            "   quit    : quit bank teller client\n");
    IO.Put ("\n");
      LOOP
        TRY
          cmd := Prompt("Command: ");

          IF Text.Equal (cmd, "account") THEN          (* select new account *)
            WITH input = Scan.Int(Prompt("   account number")) DO
              IF input < FIRST(Bank.AcctNum) OR input > LAST(Bank.AcctNum) THEN
                  RAISE InvalidAccount;
              END;
              acct := bank.findAccount(input);
              acctnum := input;
            END;

          ELSIF Text.Equal(cmd, "deposit") THEN                  (* deposit *)
            IF acct = NIL THEN RAISE InvalidAccount END;
            WITH amount = Scan.Real(Prompt("   amount")) DO
              acct.deposit(amount);
            END;

          ELSIF Text.Equal(cmd, "withdraw") THEN                (* withdraw *)
            (* make a withdrawal *)
            IF acct = NIL THEN RAISE InvalidAccount END;
            WITH amount = Scan.Real(Prompt("   amount")) DO
              acct.withdraw(amount);
            END;

          ELSIF Text.Equal(cmd, "balance") THEN             (* get a balance *)
            IF acct = NIL THEN RAISE InvalidAccount END; 
            IO.Put ("Balance is " & Fmt.Real(acct.get_balance()) & "\n");
          
          ELSIF Text.Equal(cmd, "quit") THEN  
            RAISE Quit;             (* quit by raising the "Quit" exception. *)
          
          ELSE (* invalid command *)
            IO.Put ("Valid commands are: account, deposit, withdraw, balance, and quit.\n");
          END;       

      EXCEPT

      | Bank.BadAmount => 
        IO.Put ("Cannot withdraw or deposit negative amounts.\n");

      | InvalidAccount => 
        IO.Put ("Select an account in the range [" & 
                 Fmt.Int(FIRST(Bank.AcctNum)) & ".." &
                 Fmt.Int(LAST(Bank.AcctNum))  & "] first.\n");

      | FloatMode.Trap, Lex.Error => 
        IO.Put ("Cannot convert the number as specified.\n");

      | Bank.InsufficientFunds => 
        IO.Put ("Insufficient funds available to perform this transaction\n");

      END;
    END;
  EXCEPT
  | NetObj.Error => IO.Put ("A network object error occured\n");
  | Thread.Alerted => IO.Put ("A thread was alerted\n");
  | IO.Error, Quit => IO.Put ("Goodbye.\n");
  END;
END NetObjClient.
