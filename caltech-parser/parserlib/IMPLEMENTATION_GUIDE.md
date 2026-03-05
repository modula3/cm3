# Parserlib Implementation Guide

Supplementary documentation based on building a real-world parser (`cspfe`,
a 356-rule CSP hardware description language frontend) using parserlib.
This covers practical lessons, deviations from the documented examples,
and gaps in the existing documentation that required experimentation or
source-code reading to resolve.

The reference implementation is at
`intel-async/async-toolkit/m3utils/m3utils/csp/cspparse/src/`.
An earlier, simpler reference is the `rdlparse` implementation at
`intel-async/async-toolkit/m3utils/m3utils/rdl/rdlparse/src/`.
Within the cm3 tree, the canonical example is the calculator in
`parserlib/test_parserlib2/src/`.

## 1. What the existing documentation covers well

- **Regex syntax** in `klex.html` is complete and accurate.
- **Extension file format** in `kext.html` is detailed — `$1`/`$2` syntax,
  `$$.detach()`, `%module`/`%private`/`%interface` code blocks.
- **LR(1) vs LALR(1)** discussion in `lr.html` gives good theoretical grounding.
- **The calculator example** demonstrates all four spec types working together.

## 2. Getting started: the minimal file set

The documentation (`m3build.html`) lists the m3build procedures but doesn't
provide a step-by-step workflow.  Here is the minimum file set for a working
parser:

```
myparser/src/
  my.t              # Token specification
  my.l              # Lexer specification
  my.y              # Grammar specification
  myLexExt.e        # Lexer extension (value extraction)
  myParseExt.e      # Parser extension (semantic actions)
  Main.m3           # Driver program
  m3makefile         # Build configuration
  m3overrides        # Package dependency paths
```

The corresponding `m3makefile`:

```
import ("libm3")
import ("cit_util")
import ("parserlib")

Token("my")
Lexer("my","my")
Parser("my","my")

Extended("myLexExt")
Extended("myParseExt")

implementation ("Main")
program ("myparser")
```

**Key points not in the docs:**

- Use `Token` / `Lexer` / `Parser` / `Extended` (capitalized) if you want
  the generated interfaces visible to importers.  Use lowercase if the
  parser is private to the package.
- The first argument to `Lexer()` and `Parser()` is the token spec name
  (without `.t`), not the lexer spec name.  Both arguments are typically
  the same string (e.g., `Lexer("csp","csp")`).
- `Extended()` is called once per `.e` file — you can have multiple
  extensions.
- Extension files chain: `cspLexExt.e` extends the base lexer,
  and `cspParseExt.e` extends the base parser but **imports the extended
  lexer** (`%import cspLexExt cspParse`), so it sees the value-carrying
  token fields added by the lexer extension.

## 3. The token spec (`.t` file)

### `%char` tokens

The `%char` line declares single-character tokens that can be referenced as
`'c'` in the grammar.  The documentation says `%char` tokens "can be used
both as grammar symbols and regex macros."  What this means in practice:

- In the `.y` grammar file, write `'+'`, `';'`, `'('` etc. to reference them.
- The set of characters is given as a regex character class.
- Backslash-escape `]`, `[`, `-`, `\` within the class.

**Gotcha:** Every single-character operator your language uses must be listed
in the `%char` line.  If you forget one, the lexer will match it as `ERROR`
and the parser will see unexpected tokens.  For a language with many
operators this line gets long:

```
%char [|&\^~*/%+\-?!#@<>=(){};:,.\]\[]
```

### `%const` vs `%token`

- `%const` tokens carry no value — keywords, multi-character operators.
- `%token` tokens carry values — identifiers, integers, string literals.

The documentation explains this but doesn't emphasize: **you must use
`%token` for anything whose matched text matters to semantic actions**.
In the CSP parser, only three tokens need values: `T_IDENT`, `T_INTEGER`,
`T_STRING_LIT`.  Everything else is `%const`.

### Naming conventions

Token names must be valid Modula-3 identifiers.  The convention is:

| Token type | Naming | Example |
|---|---|---|
| Keyword | `T_KEYWORD` | `T_FUNCTION`, `T_WHILE` |
| Operator | `T_OPNAME` | `T_EQ` (==), `T_ARROW` (->) |
| Value-bearing | `T_TYPE` | `T_IDENT`, `T_INTEGER` |
| Special | bare name | `ERROR`, `char`, `skip` |

Parse type names in the `.y` file **must not contain underscores** because
the generated reduction method names use the format `ruleName_returnType`.
An underscore in the return type would create ambiguity in the method name.

## 4. The lexer spec (`.l` file)

### Rule ordering is significant

The documentation mentions "default method construction" for expression
methods, but doesn't clearly state: **longer matches take priority, and
among equal-length matches, the first rule wins**.  This matters for
operators:

```
T_LSASSIGN      "<<="      # Must appear before T_LSHIFT
T_RSASSIGN      ">>="      # Must appear before T_RSHIFT
T_LEQ           "<="       # Must appear before '<'
T_GEQ           ">="       # Must appear before '>'
T_LSHIFT        "<<"       # Must appear before '<'
T_RSHIFT        ">>"       # Must appear before '>'
T_BLOOP         "<||"      # Must appear before T_OLOOP
T_OLOOP         "<|"       # Must appear before '<'
```

In practice, the lexer's longest-match semantics handles most of this
automatically, but listing longer tokens first is a defensive measure and
improves readability.

### Escape characters in regex patterns

The lexer supports standard C escape characters in character classes and
string patterns: `\a` (bell), `\b` (backspace), `\t` (tab), `\n` (newline),
`\v` (vertical tab), `\f` (form feed), `\r` (carriage return), and octal
escapes `\0xx` (three-digit octal).

### The `skip` rule

The `skip` expression method is special — it tells the lexer to discard
the matched text and continue.  Use it for whitespace and comments:

```
skip  ([ \t\r\n]|("/*"([^*]|"*"[^/])*"*/")|("//"[^\n]*"\n"))*
```

**Tip:** Include `\r` in the skip rule to handle files with CRLF line
endings (common on Windows and in files from mixed-platform projects).

**Undocumented detail:** The `skip` rule can match zero characters (via
the outer `*`).  This is fine — the lexer framework handles it correctly.

### The `ERROR` catch-all

Always end with:

```
ERROR  [^]
```

The `[^]` pattern matches any single character.  Without this, unrecognized
characters cause a runtime error rather than a parse error.

### Comments in lexer rules

Block comments (`/* ... */`) are tricky to lex correctly with regexes.
The standard pattern `([^*]|"*"[^/])*` works for non-nested comments but
cannot handle nested comments.  For the CSP lexer, this was acceptable
since CSP uses C-style non-nested comments.

If your language needs nested comments, you'll need a different approach
(perhaps treating the comment start as a token and handling nesting in
the parser or a custom lexer wrapper).

### `%macro` blocks

The documentation doesn't mention `%macro` blocks.  They work like flex
definitions — named patterns reusable in later rules:

```
%macro {
ALPHA    [a-zA-Z_]
DIGIT    [0-9]
HEX      [0-9a-fA-F]
}
```

Reference with `{MACRONAME}` in subsequent rules:

```
T_IDENT   {ALPHA}({ALPHA}|{DIGIT})*
T_INTEGER {DIGIT}+
```

## 5. The grammar spec (`.y` file)

### Rule format

The documentation explains the basic format but a real grammar has
subtleties.  Each rule set has three parts:

```
returnType:
  reductionName    symbol1 symbol2 'c' symbol3
  anotherReduction symbol1
```

The return type declaration (`returnType:`) creates both a grammar symbol
and a Modula-3 type.  The reduction names are method names that will
appear in the generated parser.

### Empty productions

To represent an optional or empty rule, use a reduction with no symbols
on the right-hand side:

```
opt_expression:
  yes    expression
  empty
```

The `empty` reduction has zero symbols — it matches nothing and always
succeeds.  **This is the standard way to handle optional constructs.**

### Operator precedence

The `%left`, `%right`, `%nonassoc` directives work like yacc but with
parserlib-specific nuances:

```
%left '|'
%left '^'
%left '&'
%left T_EQ T_NEQ
%left '<' '>' T_LEQ T_GEQ
%left T_LSHIFT T_RSHIFT
%left '+' '-'
%left '*' '/' '%'
%right T_EXP
```

**Lesson learned:** For the CSP parser, we initially tried to use
`%left`/`%right` directives for operator precedence but ended up using
the explicit precedence-climbing pattern (separate nonterminals for each
precedence level: `cond_or_expr`, `cond_and_expr`, `or_expr`, `xor_expr`,
..., `mul_expr`, `unary_expr`, `exp_expr`).

This explicit approach has two advantages:

1. **No shift/reduce conflict surprises.**  With `%left`/`%right`, adding
   new constructs (like the CSP hash-prefix operators, or statements that
   share operators with expressions) can trigger unexpected conflicts.

2. **Extension-friendly.**  Each precedence level is a separate parse type
   in the extension file, making it easy to attach different semantic actions
   at different levels.

The trade-off is verbosity — the CSP grammar has 10 expression nonterminals
where `%left`/`%right` could theoretically express the same thing in fewer
lines.  For complex languages, the explicit approach is more maintainable.

### Statement disambiguation via suffixes

When your language has statements that begin with an expression/lvalue
(assignments, function calls, channel operations), a useful pattern is
to parse the lvalue first, then branch on the suffix:

```
statement:
  lval    lvalue stmt_suffix

stmt_suffix:
  assign     '=' expression
  send       '!' opt_expression
  recv       '?' opt_lvalue
  inc        T_INC
  dec        T_DEC
  expr_stmt
```

The `expr_stmt` reduction (empty) handles bare expression statements.
This avoids shift/reduce conflicts that would arise from having separate
rules for assignment, send, receive, etc., all starting with `lvalue`.

**Caveat:** The empty `expr_stmt` reduction means the parser must
commit to the `lvalue stmt_suffix` branch as soon as it sees a token
that could start an lvalue.  This works in LR(1) as long as no other
statement form starts with the same token.

### LL(k) to LR(1) conversion

If you're converting an ANTLR (LL) grammar, the main structural changes
are:

1. **Left-recursion becomes natural.**  ANTLR grammars use right-recursion
   or EBNF `(...)*` for lists.  LR(1) handles left-recursion directly:
   ```
   expression_list:
     single    expression
     cons      expression_list ',' expression
   ```

2. **Syntactic predicates disappear.**  LL(k) grammars often need
   predicates to resolve ambiguity.  In the CSP grammar, all statement
   types were distinguishable by LR(1) lookahead after the lvalue — no
   predicates needed.

3. **`>>` tokenization.**  If your language has both `>` (comparison) and
   `>>` (shift), the ANTLR approach of treating `>>` as two `>` tokens
   creates an LR(1) shift/reduce conflict.  The solution is to make `>>`
   a single token.  The trade-off: nested angle-bracket closing (as in
   `vector<vector<int>>` in C++) requires a space: `> >`.

## 6. Extension files (`.e` files)

### Two-stage extension pattern

The standard pattern uses two extension files:

**Lexer extension** (`myLexExt.e`) — adds value fields to tokens:

```
%source my.t my.l
%import myTok myLex

T_IDENT: { val : TEXT }
T_IDENT { RETURN NEW(T_IDENT, val := $) }
```

**Parser extension** (`myParseExt.e`) — adds semantic actions:

```
%source my.t my.y
%import myLexExt myParse
```

Note: the parser extension imports `myLexExt` (not `myTok`), so it sees
the extended token types with their value fields.

### The `$` and `$1`/`$2` syntax

- In lexer extensions, `$` refers to the matched text (type `TEXT`).
- In parser extensions, `$1`, `$2`, etc. refer to the nonconstant
  symbols in the reduction, in order of appearance.

**Critical undocumented detail:** `$1`, `$2` refer only to **nonconstant**
symbols — i.e., symbols declared with `%token` in the `.t` file, or
nonterminal symbols in the grammar.  Constant tokens (`%const` and `%char`)
are skipped in the numbering.

For a rule like:

```
assignment:
  x    lvalue '=' expression
```

`$1` is `lvalue` and `$2` is `expression`.  The `'='` token is constant
and is not counted.

### Accessing the `val` field

When a parse type has a `val` field (declared via `parseType: { val : TEXT }`),
the `$n` shorthand implicitly accesses `.val`.  So:

```
$$.val := $1.val + $2.val
```

can be written as:

```
$$ := $1 + $2
```

This shorthand only works for the `val` field.  Other fields must be
accessed explicitly: `$1.cnt`, `$$.myField`.

### Using `cnt` as a secondary value

The CSP extension uses a two-field pattern — `val : TEXT` for the
S-expression string, and `cnt : INTEGER` for auxiliary bookkeeping
(statement counts, operator kinds):

```
statement: { val : TEXT; cnt : INTEGER; }
  skip  { $$.val := "skip"; $$.cnt := 1 }
  lval  { $$.val := MakeStmt($1, $2.cnt, $2); $$.cnt := 1 }
```

This pattern avoids creating separate parse types for different semantic
categories — the `cnt` field carries side-channel information through the
parse tree.

### Module-level helper procedures

For non-trivial semantic actions, define helper procedures in the
`%module` block:

```
%module {
IMPORT Text, Fmt;

PROCEDURE MakeStmt(lv : TEXT; kind : INTEGER; rhs : TEXT) : TEXT =
  BEGIN
    CASE kind OF
    | 1 => RETURN "(assign " & lv & " " & rhs & ")"
    ...
    END
  END MakeStmt;
}
```

This keeps the inline semantic actions short and readable.  The CSP
extension has ~140 lines of helper procedures in `%module` and ~460
lines of semantic actions.

### Global state variables

Module-level `VAR` declarations in `%module` persist across parse
invocations.  The CSP extension uses this for accumulated function
and structure lists:

```
VAR
  funcList  : TEXT := "";
  structList: TEXT := "";
```

**Warning:** If you call `parse()` multiple times on the same parser
instance, these variables retain their values.  Reset them in the
top-level reduction if needed.

### The `self` reference

Inside semantic actions, `self` refers to the parser instance.  The
CalcParseStd example uses this for register storage (`self.regs`).
The `%private` block adds instance fields, and `%public` adds public
fields accessible from Main.m3:

```
%public {
  scmResult : TEXT;
  scmBody   : TEXT;
}
```

After parsing, `parser.scmResult` contains the generated S-expression.

## 7. The driver program (`Main.m3`)

The documentation doesn't include a Main.m3 template.  Here is the
essential pattern:

```modula3
MODULE Main;
IMPORT FileRd, Rd, Wr, Thread, OSError;
IMPORT myLexExt, myParseExt;
FROM Stdio IMPORT stdout, stderr;
<* FATAL Wr.Failure, Thread.Alerted *>

VAR
  lexer  := NEW(myLexExt.T);
  parser := NEW(myParseExt.T);
  rd     : Rd.T;
BEGIN
  TRY
    rd := FileRd.Open(filename);
    EVAL lexer.setRd(rd);
    parser.setLex(lexer).parse().discard();
    (* Use parser.myField for results *)
  EXCEPT
  | OSError.E =>
      Wr.PutText(stderr, "cannot open file\n");
  END;
END Main.
```

Key points:

- **Create lexer and parser with `NEW`**, not by calling any factory.
- **`setRd(rd)`** initializes the lexer from a reader.
- **`setLex(lexer)`** connects parser to lexer.  Returns `T` for chaining.
- **`parse()`** runs the parser.  Returns the start symbol's `ParseType`.
  Call `.discard()` on the result if you don't need the parse tree.
- **`parse(exhaustInput := FALSE)`** stops before the first token that
  would cause a syntax error, useful for parsing embedded language blocks.
- Parse errors are reported automatically by the framework.  They appear
  on stderr and the parse call raises an exception.

### Lexer-only mode

For debugging, you can test the lexer independently using `cspTok.Test()`:

```modula3
IMPORT cspTok;
...
cspTok.Test(lexer);
```

This reads all tokens and prints them — useful for verifying that the
lexer produces the expected token stream.

## 8. Common pitfalls

### 1. Forgetting `cit_util` in imports

The `m3makefile` must `import ("cit_util")`.  The parserlib framework
depends on it.  Without it, you get opaque link errors.

### 2. Missing characters in `%char`

If your grammar references `'#'` but `#` isn't in the `%char` line of
the `.t` file, you'll get a build-time error from kyacc about an
unknown token.

### 3. Underscore in parse type names

Grammar nonterminal names like `cond_or_expr` with underscores work
fine — the underscore restriction applies only to the top-level return
type names on the left side of `:` declarations.  In practice, the CSP
grammar uses underscored names extensively for nonterminals (`det_guard_commands`,
`opt_expression_list`, etc.) without issues, because the generated method
names concatenate `reductionName_returnType` and the return types don't
have internal underscores that would be ambiguous.

Actually, this needs clarification: the restriction is that `ruleName`
(the reduction method name) combined with `returnType` must produce an
unambiguous Modula-3 method name of the form `ruleName_returnType`.  If
your return type contains underscores, it's fine as long as there's no
ambiguity.  In practice, single-word return types are simplest.

### 4. `$1` numbering skips constant tokens

This catches everyone.  Given:

```
declaration:
  x   type declarator_list
```

`$1` is `type` (first nonconstant symbol) and `$2` is `declarator_list`.
But given:

```
loop_statement:
  sloop   T_SLOOP T_IDENT ':' range ':' sequential_statement '>'
```

`$1` is `T_IDENT`, `$2` is `range`, `$3` is `sequential_statement`.
The `T_SLOOP`, `':'`, and `'>'` tokens are constant and skipped.

### 5. Literal `}` in semantic actions

Action blocks in `.e` files are delimited by `{ }`.  A literal `}` in a
string inside an action will prematurely close the block, causing a
compile error in the generated code.  For example:

```
struct_lit  { $$ := "'{" & $1 & "}" }   # BAD: } closes the action
```

**Workaround:** Avoid literal `}` in action strings.  Use alternative
representations:

```
struct_lit  { $$ := "(struct-lit " & $1 & ")" }   # OK
```

### 6. String concatenation in semantic actions

Building output strings by concatenation (`a & " " & b`) in every
semantic action is straightforward but can be slow for large parse
trees.  The CSP parser generates S-expressions up to hundreds of KB
for complex processes.  For a production compiler you might want to
accumulate into a `TextWr.T` instead, but for moderate-size inputs
the concatenation approach works fine.

### 7. Multiple start symbols

The `%start` directive can list multiple symbols.  This is useful if
your language has multiple entry points (e.g., a full program vs. a
single expression).  However, **the first symbol listed is the default**
used by `parse()`.  The documentation in `kyacc.html` mentions this
feature but doesn't explain how to select a non-default start symbol
at parse time.

## 9. Build and iteration workflow

### Initial development cycle

1. Write `.t` and `.l` first.  Build with just `Token` and `Lexer` in
   the m3makefile.  Write a small Main.m3 that calls `cspTok.Test(lexer)`
   to verify tokenization.

2. Add the `.y` grammar.  Start with a skeleton that handles the top-level
   structure.  Build with `Parser` added.  **kyacc reports LR(1)
   conflicts at build time** — read these carefully.

3. Add minimal `.e` extensions with empty semantic actions.  Build and
   test with `parse().discard()`.

4. Add semantic actions incrementally, testing after each major grammar
   section.

### Conflict resolution

When kyacc reports a shift/reduce or reduce/reduce conflict:

- **Shift/reduce:** Usually means the grammar is ambiguous at that point.
  Add `%left`/`%right` precedence if it's an operator ambiguity, or
  restructure the grammar (e.g., the suffix-disambiguation pattern for
  statements).

- **Reduce/reduce:** Usually means two rules can both match the same
  input.  This requires grammar restructuring — precedence directives
  don't help.

**Important:** kyacc resolves shift/reduce conflicts by preferring REDUCE
over SHIFT (opposite of yacc's default).  This means `opt_foo → empty`
productions can cause problems — the parser reduces `empty` immediately
rather than shifting to see what comes next.

### The deferred-decision pattern

Because kyacc prefers REDUCE over SHIFT, optional or ambiguous prefixes
must be handled by deferring the decision.  Instead of:

```
declaration:
  x  opt_type T_IDENT    # BAD: opt_type → empty always wins
```

Use a `_rest` nonterminal to defer:

```
declaration:
  ident_start  T_IDENT decl_rest

decl_rest:
  typed     T_IDENT       # First T_IDENT was the type name
  bare                     # First T_IDENT was the variable name
```

This pattern shifts the first `T_IDENT`, then uses the next token to
decide whether it was a type name or a variable name.  The SV parser
uses this extensively for port declarations, parameter declarations,
and wire declarations where the type is optional.

kyacc's error messages include the conflicting state and items, but
they can be hard to interpret for large grammars.  A useful debugging
strategy is to temporarily comment out grammar rules to isolate the
conflict.

### Testing

The CSP parser has a test suite of 116+ tests in a shell script that:
1. Generates temporary `.csp` files with test inputs
2. Runs `cspfe` on each
3. Checks for "syntax ok" or expected error messages
4. Reports pass/fail summary

This pattern (shell script generating temp files and checking exit codes)
is lightweight and effective for parser testing.

## 10. The `discard()` / `detach()` memory protocol

The documentation in `kext.html` explains the mechanics but not the
motivation.  Here's the practical summary:

- **`discard()`**: Marks a `ParseType` as reusable.  After discarding,
  the framework may recycle the object.  Call this on the result of
  `parse()` if you don't need the parse tree.

- **`detach()`**: Prevents a `ParseType` from being recycled.  Call this
  when building a parse tree — detached nodes survive past the reduction
  that created them.

- **`purge()`**: Releases all recycled `ParseType` objects.  Call this
  after you're done with the parser to free memory.

For the CSP parser, which builds S-expression strings (not a parse tree),
none of the intermediate `ParseType` objects need to be detached — the
string values are copied out by the semantic actions.  The CalcParseTree
example shows the tree-building pattern where `detach()` is essential.

## 11. Scaling considerations

The CSP grammar has 356 lines, 60+ nonterminals, and ~80 reductions.
kyacc handles this without issues — build time is under a second.

The generated parser code (in `ARM64_DARWIN/`) is roughly:
- `cspTok.m3`: ~200 lines (token definitions)
- `cspLex.m3`: ~1500 lines (DFA tables)
- `cspParse.m3`: ~3000 lines (LR(1) tables and reduction methods)

For much larger grammars, the generated tables could become significant,
but for anything up to programming-language complexity, build time and
code size are not concerns.

## 12. Summary of what's missing from the official docs

| Topic | Gap |
|---|---|
| Step-by-step tutorial | No getting-started guide for a new parser |
| `Main.m3` template | No example driver program |
| `%macro` blocks in `.l` files | Not mentioned at all |
| `$n` numbering skips constants | Mentioned obliquely, easy to misunderstand |
| Empty production pattern | Not shown explicitly |
| Statement disambiguation | No guidance on common grammar patterns |
| LL-to-LR conversion | No guidance despite many grammars being LL-origin |
| Conflict debugging | No strategies for resolving LR(1) conflicts |
| Testing patterns | No suggested test infrastructure |
| Explicit vs. directive precedence | No discussion of trade-offs |
| Global state in extensions | Not documented as a pattern |
| `self` reference in actions | Only shown in CalcParseStd, not explained in docs |

---

*Written March 2026, based on the `cspfe` implementation
(`intel-async/async-toolkit/m3utils/m3utils/csp/cspparse/`) and the
`rdlparse` implementation (`intel-async/.../rdl/rdlparse/`).*
