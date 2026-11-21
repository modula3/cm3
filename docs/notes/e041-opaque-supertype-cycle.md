# Opaque supertype cycles across units (m3tests e041)

## Problem

m3tests e041 reveals two opaque types as each other's supertype, in separate units:

```
INTERFACE One;  TYPE T1 <: ROOT;  ...
MODULE One;  IMPORT Two;  REVEAL T1 = Two.T2 BRANDED "T1" OBJECT F1: INTEGER END;  ...
MODULE Two;  IMPORT One;  REVEAL T2 = One.T1 BRANDED "T2" OBJECT F2: INTEGER END;  ...
```

Each unit checks clean on its own: the cycle exists only through the opaque "magic"
information shared between units. When cm3 recompiles a unit with that information
(`new opaque info -> recompiling One.m3`), `ObjectType.GetOffsets` calls `FindMagic`.
`FindMagic` follows the supertype chain recursively and never terminates. cm3 crashes
with a stack overflow (`Segmentation fault`, exit 139) instead of reporting an error.

## Change

`m3-sys/m3front/src/types/ObjectType.m3`:

- `HasMagicCycle` walks the supertype chain through `find_opaque_magic`, using
  Brent's cycle detection, before `FindMagic` runs. A chain that reaches a root
  (`NIL`, `ROOT`, `UNTRACED ROOT`) or a type with no magic information is not a cycle.
- On a cycle, `GetOffsets` reports `illegal recursive supertype (2.4.8).` at the
  type's position. That is the same message the front end gives for a cycle inside
  one unit.
- The root check is factored out of `FindMagic` into `IsMagicRoot`, so both procedures
  share it.

The expected e041 outputs (`stdout.build`, `stderr.build`) were empty and are now
filled in: one error per unit, then `missing "Main" module`.

## Verification

On ubuntu:24.04 (g++ 13), master and master plus this change were each built with
`concierge.py full-upgrade --backend c`, then the m3tests harness was run
(`cd m3-sys/m3tests && cm3 -D_e041`):

- **master:** e041 fails with `+Segmentation fault` in `stderr.build`.
- **with the change:** e041 passes. The output matches the new expected files exactly.
- **Rest of the suite:** 27 other tests show diffs on this host both with and without
  the change. The set is identical, so e041 is the only difference.
