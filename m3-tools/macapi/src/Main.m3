MODULE Main;

IMPORT TextRefTbl,
       TextTextTbl,
       TextSetDef,
       Lex,
       Rd,
       Wr,
       FileRd,
       FileWr,
       Text,
       ParseParams,
       Stdio,
       IO,
       Process,
       Fmt,
       Word,
       Pathname,
       TextSeq,
       Scan;

<*IMPLICIT*>
EXCEPTION Error(TEXT);

VAR
  OutDir: TEXT;                  (* output directory *)
  InDir: TEXT;                   (* input directory - only used with config
                                    file *)
  OutInt    : TEXT;              (* output interfcae file name *)
  OverFile  : TEXT;              (* override file name *)
  ConfigFile: TEXT;              (* config file name *)
  Overs := NEW(TextTextTbl.Default).init(); (* set of override
                                               definitions *)
  Env := NEW(TextSetDef.T).init(); (* the default compilation variables
                                      from the command line *)
  Defs := NEW(TextRefTbl.Default).init(); (* the database of all
                                             definitions *)
  Files := NEW(TextSeq.T).init(); (* the list of input files *)
  Units := NEW(TextSetDef.T).init(); (* the list of unit names *)
  Seperate := FALSE;             (* write seperate interface for each
                                    unit? *)
  Dump   := FALSE;               (* dump database of info *)
  Errors := 0;
  Prefix := "";


CONST
  AllChars = SET OF CHAR{FIRST(CHAR) .. LAST(CHAR)};
  Caps     = SET OF CHAR{'A' .. 'Z'};
  Lets     = SET OF CHAR{'a' .. 'z'};
  Nums     = SET OF CHAR{'0' .. '9'};
  IdChars = Caps + Lets + Nums + C{'_'}; (* legal characters for an
                                            identifier *)
  Others      = Lex.NonBlanks - IdChars; (* symbols *)
  DoLog       = TRUE;            (* write a debug log to the screen *)
  DoLogTokens = FALSE;           (* write log of tokens parsed *)

TYPE
  C = SET OF CHAR;               (* I don't much like typing *)
  Token = {Directive, Identifier, Integer, Space, Symbol, EOF};
  DefKind = {Proc, Type, Const, Var};
  Align = {Normal, Moto, Packed};

  DefInfo = REF RECORD
                  size: INTEGER;
                  unit: TEXT      := NIL;
                END;

  Parser =
    OBJECT
      in   : Rd.T         := NIL;
      out  : Wr.T         := NIL;
      token: TEXT;               (* the last token parsed *)
      vars : TextSetDef.T := NIL; (* the current compilation variables *)
      incs: TextSeq.T := NIL;    (* the units imported by the unit being
                                    parsed *)
      cstack := ARRAY [0 .. 16] OF BOOLEAN{FALSE, ..}; (* stack for nested
                                                          conditions *)
      cnests := 0;               (* the nesting depth of the condition *)
      defkind: DefKind;          (* the definition type *)
      skipall := FALSE;          (* skip everything until the next
                                    directive *)
      skipnest := 0;             (* the number of skipped nested
                                    conditions *)
      writespace := FALSE;       (* write whitespace to output returning
                                    tokens *)
      echotokens := FALSE;       (* write all tokens to the output file
                                    (including whitespace) *)
      unitname: TEXT;            (* the name of the unit being parsed *)
      unitdir: TEXT;             (* the directory in which the unit being
                                    parsed resides *)
      align: Align;              (* the alignment to use for structures *)
      override := FALSE;         (* don't translate this definition, use
                                    the override file *)
      store := FALSE;            (* store up text instead of writing it
                                    out *)
      stored: TEXT := NIL;       (* the stored text *)
    METHODS
      init      (): Parser   := ParserInit;
      parse     (path: TEXT) := ParserParse;
      nextToken (): Token    := ParserNextToken;
      match     (t: TEXT)    := ParserMatch; (* parse and discard token *)
      parseDefinition ()                        := ParserParseDefinition;
      parseDirective  ()                        := ParserParseDirective;
      write           (t: TEXT)                 := ParserWrite;
      writeHead       ()                        := ParserWriteHead;
      writeFoot       ()                        := ParserWriteFoot;
      writeStored     ()                        := ParserWriteStored;
      peek            (): CHAR                  := ParserPeek;
      skipTo          (c: CHAR)                 := ParserSkipTo;
      skipTokensTo    (t: TEXT; write := FALSE) := ParserSkipTokensTo;
      skipBlanks      ()                        := ParserSkipBlanks;
    END;

(*** Parser Methods ***)

PROCEDURE ParserWrite (this: Parser; t: TEXT) =
  BEGIN
    IF NOT this.override THEN
      IF NOT this.store THEN
        Wr.PutText(this.out, t);
      ELSE
        IF this.stored = NIL THEN
          this.stored := t;
        ELSE
          this.stored := this.stored & t;
        END;
      END;
    END;
  END ParserWrite;

PROCEDURE ParserWriteStored (this: Parser) =
  BEGIN
    IF this.stored # NIL THEN
      Wr.PutText(this.out, this.stored);
      this.stored := NIL;
    END;
  END ParserWriteStored;

PROCEDURE ParserWriteHead (this: Parser) =
  BEGIN
    this.write("INTERFACE " & this.unitname & ";\n\n");
    IF Seperate THEN
      WITH ci = this.incs.size() - 1 DO
        this.write("IMPORT\n\t");
        FOR i := 0 TO ci DO
          this.write("\n\t\t\t" & this.incs.get(i) & ".p");
          IF i # ci THEN this.write(", "); ELSE this.write(";\n\n"); END;
        END;
      END;
    END;
    this.write("<*PRAGMA ALIGN*>\n\n");
    this.write(Prefix & "\n");
  END ParserWriteHead;

PROCEDURE ParserWriteFoot (this: Parser) =
  BEGIN
    this.write("\nEND " & this.unitname & ".\n");
  END ParserWriteFoot;

PROCEDURE ParserMatch (this: Parser; t: TEXT) =
  VAR tok := "";
  BEGIN
    IF Text.GetChar(t, 0) IN Others THEN
      (* symbol tokens are always one char long *)
      FOR i := 0 TO Text.Length(t) - 1 DO
        EVAL this.nextToken();
        tok := tok & this.token;
      END;
    ELSE
      EVAL this.nextToken();
      tok := this.token;
    END;
    IF NOT Text.Equal(t, tok) THEN
      RAISE
        Error("syntax error: expected '" & t & "' but got '" & tok & "'");
    END;
  END ParserMatch;

PROCEDURE ParserParseDefinition (this: Parser) =

  (* parsing procedures *)

  PROCEDURE Proc () =
    BEGIN
      this.skipBlanks();
      IF this.peek() = '(' THEN
        (* has a signatiure *)
        this.match("(");
        this.write("(");
        LOOP
          (* check for "..." *)
          this.skipBlanks();
          IF this.peek() = '.' THEN
            this.match("...");
            IF NOT this.override AND NOT bad THEN
              Log("Error: Unhandled declaration " & name
                    & " (variable parameters) ");
              bad := TRUE;
              INC(Errors);
            END;
            this.match(")");
            this.write(")");
            EXIT;
          END;
          (* scan up to the colon *)
          this.skipTokensTo(":", write := TRUE);
          (* read the type *)
          Type();
          EVAL this.nextToken();
          IF Text.Equal(this.token, ")") THEN this.write(")"); EXIT; END;
          IF NOT Text.Equal(this.token, ";") THEN
            RAISE Error("syntax error: expected ';' but got '" & this.token
                          & "'");
          END;
          this.write(";");
        END;
        this.skipBlanks();
      ELSE
        this.write("()");
      END;
      IF this.peek() = ':' THEN
        (* has a return value *)
        this.match(":");
        this.write(":");
        Type();
      END;
    END Proc;

  PROCEDURE Type () =
    BEGIN
      EVAL this.nextToken();
      IF Text.Equal(this.token, "RECORD") THEN
        struct := TRUE;
        this.write(this.token);
        LOOP
          EVAL this.nextToken();
          (* is it a variant record? *)
          IF Text.Equal(this.token, "CASE") THEN
            IF NOT this.override THEN
              IF NOT bad THEN
                Log("Error: Unhandled declaration " & name
                      & " (variant record) ");
                bad := TRUE;
                INC(Errors);
              END;
            END;
            (* skip to the end of the declaration *)
            this.write(this.token);
            VAR s := 1;
            BEGIN
              REPEAT
                IF this.nextToken() = Token.Identifier THEN
                  IF Text.Equal(this.token, "RECORD") THEN
                    INC(s);
                  ELSIF Text.Equal(this.token, "END") THEN
                    DEC(s)
                  END;
                END;
                this.write(this.token);
              UNTIL s = 0;
            END;
            EXIT;
          END;
          (* is it the end of the record? *)
          IF Text.Equal(this.token, "END") THEN
            this.write(this.token);
            EXIT;
          END;
          (* parse record entry ident ':' type ';' *)
          this.write(this.token); (* ident *)
          this.match(":");
          this.write(":");
          Type();
          this.match(";");
          this.write(";");
        END;
      ELSIF Text.Equal(this.token, "ARRAY") THEN
        struct := TRUE;
        this.write(this.token);
        this.match("[");
        LOOP
          Type();
          EVAL this.nextToken();
          IF Text.Equal(this.token, ",") THEN
            this.write(",");
          ELSIF Text.Equal(this.token, "]") THEN
            EXIT;
          ELSE
            RAISE Error("syntax error: expected ',' or ']' but got '"
                          & this.token & "'");
          END;
        END;
        this.match("OF");
        this.write("OF");
        Type();
      ELSIF Text.Equal(this.token, "PROCEDURE")
              OR Text.Equal(this.token, "FUNCTION") THEN
        (* procedural type *)
        this.write("PROCEDURE ");
        Proc();
      ELSIF Text.Equal(this.token, "STRING") THEN
        (* STRING[size] *)
        this.match("[");
        EVAL this.nextToken();
        struct := TRUE;
        this.write("ARRAY [0.." & this.token & "] OF ");
        this.write("CHAR");
        this.match("]");
      ELSIF Text.Equal(this.token, "SET") THEN
        (* scalar set *)
        this.match("OF");
        Type();
      ELSIF Text.Equal(this.token, "(") THEN
        (* enumeration *)
        this.write("{");
        LOOP
          EVAL this.nextToken();
          IF Text.Equal(this.token, ")") THEN EXIT END;
          this.write(this.token);
        END;
        this.write("}");
      ELSIF Text.Equal(this.token, "PACKED") THEN
        (* packing alignment on *)
        this.align := Align.Packed;
        Type();
      ELSIF Text.Equal(this.token, "UNIV") THEN
        (* discard *)
        Type();
      ELSIF Text.Equal(this.token, "^") THEN
        (* pointer *)
        this.write("UNTRACED REF ");
        Type();
      ELSIF Text.Equal(this.token, "INTEGER") THEN
        (* to avoid a clash with M3's INTEGER *)
        this.write("Integer");
      ELSE
        (* identifier or subrange *)
        VAR
          p := "";
          t := this.token;
        BEGIN
          IF Text.Equal(t, "+") OR Text.Equal(t, "-") OR Text.Equal(t, "$") THEN
            (* a integer literal *)
            p := this.token;
            EVAL this.nextToken();
            t := this.token;
          END;
          this.skipBlanks();
          IF this.peek() = '.' THEN
            (* a subrange *)
            this.match("..");
            (* scan until "," or "]" or ";" just in case it's a constant
               expression *)
            WITH t2 = Lex.Scan(this.in, AllChars - C{',', ']', ';'}) DO
              this.write("[" & p & t & ".." & t2 & "]");
            END;
          ELSE
            (* just an identifier *)
            this.write(this.token);
          END;
        END;
      END;
    END Type;

  PROCEDURE Const () =
    BEGIN
      LOOP
        EVAL this.nextToken();
        IF Text.Equal(this.token, ";") THEN
          IF paren = 0 THEN EXIT END;
          this.write(";");
        ELSIF Text.Equal(this.token, "$") THEN
          this.write("16_");
        ELSIF Text.Equal(this.token, "'") THEN
          (* a string *)
          WITH t = Lex.Scan(this.in, AllChars - C{'\''}) DO
            EVAL Rd.GetChar(this.in); (* skip closing quote *)
            IF Text.Length(t) = 4 THEN
              (* if it has four chars, we assume it's a "four char int" *)
              WITH i = ORD(Text.GetChar(t, 0))
                         + Word.Shift(ORD(Text.GetChar(t, 1)), 8)
                         + Word.Shift(ORD(Text.GetChar(t, 2)), 16)
                         + Word.Shift(ORD(Text.GetChar(t, 3)), 24) DO
                this.write("16_" & Fmt.Unsigned(i) & " (* converted from '"
                             & t & "' *) ");
              END;
            ELSE
              this.write("\"" & t & "\"");
            END;
          END;
        ELSE
          this.write(this.token);
        END;
      END;
    END Const;

  PROCEDURE AlignPragma () =
    BEGIN
      IF NOT struct THEN RETURN END;
      (* write out the pragma for the current alignment *)
      CASE this.align OF
      | Align.Moto => this.write("<*ALIGN M68K*> ");
      | Align.Packed => this.write("<*ALIGN PACKED*> ");
      ELSE
      END;
    END AlignPragma;

  VAR
    paren := 0;                  (* depth of brackets *)
    struct := FALSE;             (* is this a structure or does it contain
                                    a structure? *)
    name := this.token;
    info := NEW(DefInfo);
    bad  := FALSE;               (* unhandled declaration *)
  BEGIN
    IF Text.GetChar(name, 0) = '_' THEN
      (* M3 does not allow identifiers starting with an underscore *)
      name := "a" & name;
    END;

    (* Log(name); *)

    IF Defs.put(name, info) THEN
      RAISE Error("Duplicate Definition: '" & name & "'");
    END;

    CASE this.defkind OF
    | DefKind.Proc =>
        this.write("\t" & name);
        VAR d: TEXT;
        BEGIN
          this.override := Overs.get(name, d);
        END;
        IF NOT this.override THEN this.writespace := TRUE; END;
        Proc();
        this.match(";");
        this.write(";\n");
        this.writespace := FALSE;

    | DefKind.Const =>
        this.write("\t" & name);
        this.match("=");
        this.write(" = ");
        VAR d: TEXT;
        BEGIN
          this.override := Overs.get(name, d);
        END;
        IF NOT this.override THEN this.writespace := TRUE; END;
        Const();
        this.writespace := FALSE;
        this.write(";\n");

    | DefKind.Type =>
        VAR oldalign := this.align;
        BEGIN
          this.store := TRUE;
          this.write("\t" & name);
          this.match("=");
          this.write(" = ");
          VAR d: TEXT;
          BEGIN
            this.override := Overs.get(name, d);
          END;
          IF NOT this.override THEN this.writespace := TRUE; END;
          Type();
          this.match(";");
          this.writespace := FALSE;
          this.store := FALSE;
          AlignPragma();
          this.writeStored();
          this.write(";\n");
          this.align := oldalign;
        END;

    | DefKind.Var =>
        this.override := TRUE;
        this.match(":");
        Type();
        this.match(";");
        this.override := FALSE;

    END;
    IF this.override THEN
      (* write the replacement definition *)
      this.override := FALSE;
      VAR d: TEXT;
      BEGIN
        EVAL Overs.get(name, d);
        this.write(d);
      END;
      this.write(";\n");
    END;

  END ParserParseDefinition;

PROCEDURE ParserParseDirective (this: Parser) =
  VAR tok: Token;

  (* expression parser functions *)

  PROCEDURE Expr2 (): BOOLEAN =
    BEGIN
      IF tok = Token.Integer THEN
        (* literal *)
        WITH r = (Scan.Int(this.token) # 0) DO
          tok := this.nextToken();
          RETURN r;
        END;
      ELSIF Text.Equal(this.token, "(") THEN
        (* ( expr ) *)
        tok := this.nextToken();
        WITH v = Expr() DO
          IF NOT Text.Equal(this.token, ")") THEN
            RAISE Error("was expecting ')', got '" & this.token & "'");
          END;
          tok := this.nextToken();
          RETURN v;
        END;
      ELSIF Text.Equal(Upper(this.token), "UNDEFINED") THEN
        (* UNDEFINED var *)
        IF this.nextToken() # Token.Identifier THEN
          RAISE
            Error("was expecting a variable, got '" & this.token & "'");
        ELSE
          WITH r = NOT this.vars.member(this.token) DO
            tok := this.nextToken();
            RETURN r;
          END;
        END;
      ELSIF Text.Equal(Upper(this.token), "NOT") THEN
        (* NOT expr *)
        tok := this.nextToken();
        WITH r = NOT Expr() DO RETURN r; END;
      ELSE
        (* variable *)
        WITH r = this.vars.member(this.token) DO
          tok := this.nextToken();
          RETURN r;
        END;
      END;
    END Expr2;

  PROCEDURE Expr1 (): BOOLEAN =
    VAR v: BOOLEAN;
    BEGIN
      v := Expr2();
      WHILE Text.Equal(Upper(this.token), "OR") DO
        tok := this.nextToken();
        WITH r = Expr2() DO v := v OR r; END;
      END;
      RETURN v;
    END Expr1;

  PROCEDURE Expr (): BOOLEAN =
    VAR v: BOOLEAN;
    BEGIN
      v := Expr1();
      WHILE Text.Equal(Upper(this.token), "AND") DO
        tok := this.nextToken();
        WITH r = Expr1() DO v := v AND r; END;
      END;
      RETURN v;
    END Expr;

  BEGIN
    IF this.skipall THEN
      IF NOT Text.Equal(Upper(this.token), "ENDC")
           AND NOT Text.Equal(Upper(this.token), "ELSEC")
           AND NOT Text.Equal(Upper(this.token), "IFC") THEN
        this.skipTo('}');
        RETURN;
      END;
    END;
    (* Log("> Directive = " & this.token); *)
    IF Text.Equal(this.token, "SETC") THEN
      (* set a variable: SETC var := var | literal *)
      EVAL this.nextToken();
      VAR target := this.token;
      BEGIN
        this.match(":=");
        IF this.nextToken() = Token.Integer THEN
          (* set to a literal *)
          IF Scan.Int(this.token) # 0 THEN
            EVAL this.vars.insert(target);
          ELSE
            EVAL this.vars.delete(target);
          END;
        ELSIF Text.Equal(this.token, "OPTION") THEN
          (* OPTION function - always returns false *)
          this.match("(");
          tok := this.nextToken(); (* skip identifier *)
          this.match(")");
          EVAL this.vars.delete(target);
        ELSE
          (* set to another variable *)
          IF this.vars.member(this.token) THEN
            EVAL this.vars.insert(target);
          ELSE
            EVAL this.vars.delete(target);
          END;
        END;
      END;
    ELSIF Text.Equal(this.token, "IFC") THEN
      (* test a condition: IFC expression

         expression: 'UNDEFINED' variable 'NOT' expression variable literal
         '(' expression ')' expression 'AND' expression expression 'OR'
         expression

         eliminating left recursion we get: expr: expr1 ('AND' expr1)*
         expr1: expr2 ('OR' expr2)* expr2: variable literal 'UNDEFINED'
         variable '(' expr ')' 'NOT' expr *)

      (* if we are skipping, just increment nesting *)
      IF this.skipall THEN
        INC(this.skipnest);
        (* Log(">>> "&Fmt.Int(this.cnests)); *)
        this.skipTo('}');
        RETURN;
      END;

      tok := this.nextToken();
      WITH r = Expr() DO
        INC(this.cnests);
        (* Log(">>> "&Fmt.Int(this.cnests)); *)
        this.cstack[this.cnests - 1] := r;
        IF NOT Text.Equal(this.token, "}") THEN
          RAISE Error("expecting '}' but got '" & this.token & "'");
        END;
        this.skipall := NOT r;
      END;
    ELSIF Text.Equal(this.token, "ELSEC") THEN
      (* else clause of conditional *)
      IF this.skipnest = 0 THEN
        this.skipall := this.cstack[this.cnests - 1];
      END;
      this.skipTo('}');
    ELSIF Text.Equal(this.token, "ENDC") THEN
      (* end of conditional *)
      IF this.skipnest = 0 THEN
        this.skipall := FALSE;
        DEC(this.cnests);
        (* Log(">>> "&Fmt.Int(this.cnests)); *)
        IF this.cnests < 0 THEN
          RAISE Error("unbalanced IF directives");
        END;
      ELSE
        DEC(this.skipnest);
        IF this.skipnest < 0 THEN
          RAISE Error("unbalanced IF directives");
        END;
      END;
      this.skipTo('}');
    ELSIF Text.Equal(this.token, "I") THEN
      (* unit inclusion: I <filename> *)
      IF this.nextToken() # Token.Symbol THEN
        this.incs.addhi(this.token);
        (* check to see if we are processing this file, if not add it to
           the list *)
        (* FIXME - temparary hack *)
        IF NOT Units.member(this.token) THEN
          WITH n = this.unitdir & "/" & this.token & ".p" DO
            Files.addhi(n);
            EVAL Units.insert(this.token);
            (* Log("Including " & n); *)
          END;
        END;
      END;
      this.skipTo('}');
    ELSIF Text.Equal(this.token, "ALIGN") THEN
      (* aligment pragma ALIGN RESET | MAC68K | PPC *)
      EVAL this.nextToken();
      IF Text.Equal(this.token, "MAC68K") THEN
        this.align := Align.Moto;
      ELSE
        this.align := Align.Normal;
      END;
      this.skipTo('}');
    ELSIF Text.Equal(this.token, "PUSH") THEN
      (* useful as a signal to that list of includes has finished *)
      IF Seperate THEN this.writeHead(); END;
    ELSE
      this.skipTo('}');
    END;
  END ParserParseDirective;

PROCEDURE ParserParse (this: Parser; path: TEXT) =
  BEGIN
    this.unitname := Pathname.LastBase(path);
    this.unitdir := Pathname.Prefix(path);
    this.incs := NEW(TextSeq.T).init();
    this.vars := Env.copy();
    this.in := FileRd.Open(path);
    (* open output file *)
    IF Seperate THEN
      (* create a new M3 Interface file *)
      this.out := FileWr.Open(OutDir & this.unitname & ".m3");
    END;

    LOOP
      WITH token = this.nextToken() DO
        IF this.echotokens THEN this.write(this.token); END;
        IF NOT this.skipall OR token = Token.Directive OR token = Token.EOF THEN
          CASE token OF
          | Token.Directive =>
              this.token := Upper(this.token);
              this.parseDirective();
          | Token.Identifier =>
              IF Text.Equal(this.token, "TYPE") THEN
                this.defkind := DefKind.Type;
                this.write("\nTYPE\n");
              ELSIF Text.Equal(this.token, "VAR") THEN
                this.defkind := DefKind.Var;
              ELSIF Text.Equal(this.token, "CONST") THEN
                this.defkind := DefKind.Const;
                this.write("\nCONST\n");
              ELSIF Text.Equal(this.token, "FUNCTION") THEN
                this.defkind := DefKind.Proc;
                this.write("<*EXTERNAL*> PROCEDURE ");
              ELSIF Text.Equal(this.token, "PROCEDURE") THEN
                this.defkind := DefKind.Proc;
                this.write("<*EXTERNAL*> PROCEDURE ");
              ELSIF Text.Equal(this.token, "UNIT") THEN
                this.skipTo(';');
              ELSIF Text.Equal(this.token, "INLINE") THEN
                this.skipTo(';');
              ELSIF Text.Equal(this.token, "INTERFACE") THEN
              ELSIF Text.Equal(this.token, "C") THEN
                this.skipTo(';');
              ELSIF Text.Equal(this.token, "END") THEN
                this.skipTo('.');
              ELSE
                this.parseDefinition();
              END;
          | Token.EOF =>
              (* close input file *)
              Rd.Close(this.in);
              this.in := NIL;
              (* close output file *)
              IF Seperate THEN
                this.writeFoot();
                Wr.Close(this.out);
                this.out := NIL;
              END;

              RETURN;
          ELSE
            (* FIXME: raise error *)
          END;
        END;
      END;
    END;

    (* write footer *)

    (* close output file *)

  END ParserParse;

PROCEDURE ParserNextToken (this: Parser): Token =
  BEGIN
    LOOP
      IF Rd.EOF(this.in) THEN this.token := ""; RETURN Token.EOF; END;
      WITH c = this.peek() DO
        IF c IN (Caps + Lets + C{'_'}) THEN
          this.token := Lex.Scan(this.in, IdChars);
          IF DoLogTokens THEN Log("'" & this.token & "'") END;
          RETURN Token.Identifier;
        ELSIF c IN C{'0' .. '9'} THEN
          this.token := Lex.Scan(this.in, Nums);
          IF DoLogTokens THEN Log("'" & this.token & "'") END;
          RETURN Token.Integer;
        ELSIF c IN Lex.Blanks THEN
          this.skipBlanks();
        ELSE
          (* symbolic *)
          IF c = '{' THEN
            EVAL Rd.GetChar(this.in);
            WITH nc = this.peek() DO
              IF nc = '$' THEN
                (* directive *)
                EVAL Rd.GetChar(this.in); (* skip '$' *)
                this.token := Lex.Scan(this.in, IdChars);
                IF DoLogTokens THEN Log("'" & this.token & "'") END;
                RETURN Token.Directive;
              ELSE
                (* comment *)
                this.skipTo('}');
              END;
            END;
          ELSE
            EVAL Rd.GetChar(this.in);
            this.token := Text.FromChar(c);
            IF DoLogTokens THEN Log("'" & this.token & "'") END;
            RETURN Token.Symbol;
          END;
        END;
      END;
    END;
  END ParserNextToken;

PROCEDURE ParserPeek (this: Parser): CHAR =
  BEGIN
    WITH c = Rd.GetChar(this.in) DO Rd.UnGetChar(this.in); RETURN c; END;
  END ParserPeek;

PROCEDURE ParserSkipTo (this: Parser; c: CHAR) =
  BEGIN
    Lex.Skip(this.in, AllChars - C{c});
    EVAL Rd.GetChar(this.in);
  END ParserSkipTo;

PROCEDURE ParserSkipTokensTo (this: Parser; t: TEXT; write := FALSE) =
  BEGIN
    REPEAT
      EVAL this.nextToken();
      IF write THEN this.write(this.token) END;
    UNTIL Text.Equal(this.token, t);
  END ParserSkipTokensTo;

PROCEDURE ParserSkipBlanks (this: Parser) =
  BEGIN
    WITH b = Lex.Scan(this.in, Lex.Blanks) DO
      IF this.writespace THEN this.write(b); END;
    END;
    (* FIXME: needs to handle comments as whitepace *)
  END ParserSkipBlanks;

PROCEDURE ParserInit (this: Parser): Parser =
  BEGIN
    RETURN this;
  END ParserInit;


(*** Utils ***)

PROCEDURE Log (t: TEXT) =
  BEGIN
    IF NOT DoLog THEN RETURN END;
    IO.Put(t & "\n");
  END Log;

PROCEDURE Upper (t: TEXT): TEXT =
  VAR b := NEW(REF ARRAY OF CHAR, Text.Length(t));
  BEGIN
    FOR i := 0 TO LAST(b^) DO
      WITH c = Text.GetChar(t, i) DO
        IF c IN Lets THEN
          b[i] := VAL(ORD(c) - 32, CHAR);
        ELSE
          b[i] := c;
        END;
      END;
    END;
    RETURN Text.FromChars(b^);
  END Upper;


(*** Main Procs ***)

PROCEDURE GetParams () =
  CONST usage = "";
  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      TRY
        (* output directory *)
        IF pp.keywordPresent("-outdir") THEN
          OutDir := pp.getNext();
        ELSE
          OutDir := ".";
        END;

        (* input directory *)
        IF pp.keywordPresent("-indir") THEN
          InDir := pp.getNext();
        ELSE
          InDir := ".";
        END;

        (* output interface *)
        IF pp.keywordPresent("-outint") THEN
          OutInt := pp.getNext();
        ELSE
          OutInt := "Mac";
        END;

        (* override file *)
        IF pp.keywordPresent("-overrides") THEN
          OverFile := pp.getNext();
        ELSE
          OverFile := NIL;
        END;

        (* config file *)
        IF pp.keywordPresent("-config") THEN
          ConfigFile := pp.getNext();
        ELSE
          ConfigFile := NIL;
        END;

        (* symbol definitions *)
        WHILE pp.keywordPresent("-def") DO
          EVAL Env.insert(pp.getNext());
        END;

        (* seperate interfaces *)
        IF pp.keywordPresent("-seperate") THEN
          RAISE Error("-seperate is unimplemented");
        END;

        (* dump database *)
        IF pp.keywordPresent("-dump") THEN Dump := TRUE; END;

        (* list of input files *)
        pp.skipParsed();
        WITH n = NUMBER(pp.arg^) - pp.next DO
          FOR i := 0 TO n - 1 DO Files.addhi(pp.getNext()); END
        END;

        (* Check for any unparsed parameters: *)
        pp.finish();

      EXCEPT
        ParseParams.Error =>
          Wr.PutText(Stdio.stderr, usage);
          Process.Exit(1);
      END
    END
  END GetParams;

PROCEDURE LoadOverrides () =
  BEGIN
    IF OverFile = NIL THEN RETURN END;
    WITH in = FileRd.Open(OverFile) DO
      Lex.Skip(in, AllChars - C{'#'});
      EVAL Rd.GetChar(in);
      LOOP
        VAR name, body: TEXT := NIL;
        BEGIN
          TRY
            TRY
              name := Rd.GetLine(in);
              body := Lex.Scan(in, AllChars - C{'#'});
              EVAL Rd.GetChar(in);
            FINALLY
              IF name # NIL THEN
                IF Text.Equal(name, "") THEN
                  Prefix := body;
                ELSE
                  EVAL Overs.put(name, body);
                  (* Log("Added override for '" & name & "'"); *)
                END;
              END;
            END;
          EXCEPT
            Rd.EndOfFile => EXIT;
          END;
        END;
      END;
    END;
  END LoadOverrides;

PROCEDURE LoadConfig () =
  VAR
    c: CHAR;
    l: TEXT;
  BEGIN
    IF ConfigFile = NIL THEN RETURN END;
    WITH in = FileRd.Open(ConfigFile) DO
      LOOP
        TRY
          (* Lex.Skip(in, AllChars-C{'#','$','%'}); *)
          c := Rd.GetChar(in);
          l := Rd.GetLine(in);
          (* FIXME: strip trailing and leading spaces *)
        EXCEPT
          Rd.EndOfFile => EXIT;
        END;
        CASE c OF
        | '%' =>
          (* comment *)
        | '$' =>
            (* Log("Directive: '" & l & "'"); *)
            EVAL Env.insert(l);
        | '#' =>
            (* Log("Unit: '" & l & "'"); *)
            Files.addhi(InDir & "/" & l)
        ELSE
        END;
      END;
    END;
  END LoadConfig;

PROCEDURE ParseUnits () =
  VAR
    parser := NEW(Parser).init();
    done   := 0;
  BEGIN
    FOR i := 0 TO Files.size() - 1 DO
      EVAL Units.insert(Pathname.LastBase(Files.get(i)));
    END;

    IF NOT Seperate THEN
      parser.out := FileWr.Open(OutDir & "/" & OutInt & ".i3");
      parser.unitname := OutInt;
      parser.writeHead();
    END;

    (*
    parser.parse(Pathname.Prefix(Files.get(0)) & "/" & "ConditionalMacros.p");
    Env := parser.vars.copy();
    *)

    (* parse the input files *)
    REPEAT
      Log("Parsing " & Files.get(done));
      parser.parse(Files.get(done));
      INC(done);
    UNTIL done = Files.size();
    IF NOT Seperate THEN
      parser.unitname := OutInt;
      parser.writeFoot();
      Wr.Close(parser.out);
      parser.out := NIL;
    END;
  END ParseUnits;

PROCEDURE DumpData () =
  VAR c: TEXT;
  BEGIN
    IF NOT Dump THEN RETURN END;
    (* write tab-delimited files of data gathered *)
    Log("\n\nStats:");
    Log("- units: " & Fmt.Int(Files.size()));
    Log("- definitions: " & Fmt.Int(Defs.size()));
    Log("- errors: " & Fmt.Int(Errors));

    Log("\n\nConditionals:");
    WITH it = Env.iterate() DO WHILE it.next(c) DO Log("\t" & c); END; END;

    Log("\n\nUnits:");
    WITH it = Units.iterate() DO
      WHILE it.next(c) DO Log("\t" & c); END;
    END;
  END DumpData;

PROCEDURE Init () =
  BEGIN
    Env := NEW(TextSetDef.T).init();
    EVAL Env.insert("UsingIncludes");

    (* get command line *)
    GetParams();

    (* load the config file if specified *)
    LoadConfig();

    (* load the override file if specified *)
    LoadOverrides();

  END Init;

PROCEDURE Fin () =
  BEGIN
    DumpData();
  END Fin;

BEGIN
  TRY
    Init();
    ParseUnits();                (* parse and process the input files *)
    Fin();
  EXCEPT
    Error (t) => Log("ERROR: " & t);
  END;
END Main.
