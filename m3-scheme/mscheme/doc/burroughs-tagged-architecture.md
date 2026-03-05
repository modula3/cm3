# Burroughs Large Systems: Tagged Architecture and Inline Polish

Notes on the Burroughs tagged-word architecture, compiled during
research for the MScheme interoperability report.

## Tagged Words

The Burroughs B5000 (1961) used a single flag bit in its 48-bit word
to distinguish code words from data words.  The B6500/B6700 (1969)
expanded this to 3-bit tags on every 51-bit word:

| Tag | Category                     | Description                                    |
|-----|------------------------------|------------------------------------------------|
| 0   | Single-precision data        | User data: text, single-precision numbers      |
| 1   | IRW / SIRW                   | Indirect Reference Word (on-stack data address) |
| 2   | Double-precision data        | Double-precision numbers                       |
| 3   | Code / control words         | Program code; stack control words (MSCW, RCW)  |
| 4   | SIW                          | Step Index Word (FOR loop control)             |
| 5   | Descriptor                   | Off-stack data addresses, array/segment descriptors |
| 6   | Uninitialized / SCW          | Uninitialized data or Software Control Word    |
| 7   | PCW                          | Program Control Word (procedure entry point)   |

Even-tagged words (0, 2, 4, 6) are user-modifiable data.  Odd-tagged
words (1, 3, 5, 7) are control/system words that user code cannot
write -- enforced in hardware.  This prevents user code from forging
pointers, corrupting return addresses, or creating fake descriptors.

Descriptors (tag 5) are bounds-checked capabilities: they contain the
address, length, and type of a data segment, and the hardware
bounds-checks every access through them automatically.

The tags served *machine-level integrity*, not language-level type
dispatch.  The hardware distinguished "this is a data word" from "this
is a pointer" from "this is a return address," but it did not
distinguish user-defined types like "this is a Scheme pair" vs. "this
is a Scheme symbol."  That is the key difference from the Lisp
Machines, where the tags encoded Lisp types directly.

## The Burroughs as an Algol Machine

The Burroughs primary language was ALGOL 60 (later Extended ALGOL).
The machine code was a Reverse Polish Notation syllable stream
("Polish string") -- 12-bit syllables on the B5000, 8-bit on the
B6500 and later -- emitted directly by the ALGOL compiler.  There
was deliberately no assembler; all code, including the operating
system, was written in high-level languages.

## Inline Machine Code: ESPOL and "Set Serendipity"

**ESPOL** (Executive Systems Problem Oriented Language) was a superset
of ALGOL 60 used to write the MCP (Master Control Program).  It
exposed hardware syllables as intrinsic functions:

- `TOPOFSTACK` -- references the top of the hardware evaluation stack;
  can appear wherever a word variable can: `TOPOFSTACK := A := I+J;`
- `EXCHANGE(V)` -- maps to the EXCH operator
- `READLOCK` -- maps to the RDLK operator (multiprocessor semaphore):
  `x := RDLK(x, y);`
- `MOVESTACK(N)` -- transfers control to stack N
- `SCANIN(N)` / `SCANOUT(N)` -- maps to SCNI/SCNO operators

ESPOL also had a `DEFINE` mechanism (suggested by Donald Knuth) for
compile-time text substitution that generated inline code.

**"Set serendipity"** was an unofficial modification to the B6700
ALGOL compiler (by Bill Thomas) that added all hardware opcode
mnemonics to the compiler's symbol table.  With this option enabled,
assembler source statements could be mixed with normal ALGOL source
and refer to ALGOL-declared variables: e.g., `VALC X` where X was an
ALGOL variable.  The name was chosen deliberately so it "would not be
stumbled upon" by ordinary users.

**NEWP** replaced ESPOL in the mid-to-late 1970s.  It retained
machine-level access but added security controls: unsafe constructs
required blocks to be specifically marked, and such programs needed
a security administrator to "bless" them before they could execute.
NEWP also supported `INLINE` procedure declarations.

These mechanisms are all forms of *inline assembly* -- dropping to a
lower abstraction level within the same language -- not language
interoperability in the MScheme sense.

## Lisp on Burroughs

Interlisp was ported to the B6700 at UCSD in 1974-1975 (Bill Gord and
Stan Sieler, under Ken Bowles, funded by DARPA).  This was a
straightforward port -- Lisp running as its own program on MCP -- not
an embedding of Lisp inside ALGOL.

## Relevance to MScheme

The Burroughs tagged architecture is an important historical precedent
for hardware-enforced type tagging, predating the Lisp Machines by
over a decade.  But the Burroughs tags and the Lisp Machine tags
served different purposes:

- Burroughs: machine-level integrity (code vs. data vs. pointer vs.
  return address).  An Algol machine.
- Lisp Machines: language-level type dispatch (fixnum vs. cons vs.
  symbol vs. closure).  A Lisp machine.
- Modula-3/MScheme: language-level type dispatch via `REFANY` +
  `TYPECASE`, implemented in software (compiler-generated metadata),
  running on conventional untagged hardware.

## References

- Organick, E.I.  *Computer System Organization: The B5700/B6700
  Series.*  Academic Press, 1973.

- Levy, H.M.  *Capability-Based Computer Systems.*  Digital Press,
  1984.  Chapter 2 covers the Burroughs B5000 and B6500.

- B6700 ESPOL Information Manual.  Burroughs document 5000094,
  June 1972.

- A Series NEWP Programming Reference Manual.  Unisys, May 1989.
