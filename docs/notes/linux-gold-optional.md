# Gold linker optional on Linux

## Problem

`m3-sys/cminstall/src/config/Linux.common` forces the gold linker on every link:

```
LD_PREF = " -fuse-ld=gold"
```

binutils 2.44 and later may ship without `ld.gold`; Fedora 44 has none. There every
link fails with `collect2: fatal error: cannot find 'ld'`, so a full upgrade cannot
complete.

## Change

```
if not defined ("LD_PREF")
  if fs_exists ("/usr/bin/ld.gold")
    LD_PREF = " -fuse-ld=gold"
  else
    LD_PREF = ""
  end
end
```

- **Gold installed:** cm3 still links with gold, so nothing changes.
- **Gold missing:** cm3 falls back to the system's default linker. The flags that
  follow `LD_PREF` in `SYSTEM_LD` each start with their own space, so an empty value
  is safe.
- **Override:** an already-defined `LD_PREF` wins, e.g.
  `cm3 -DLD_PREF=" -fuse-ld=lld"`. Before, choosing a linker meant editing the file.

## Limits

- Only `/usr/bin/ld.gold` is checked. Gold installed elsewhere is not detected, and
  the build uses the default linker, which is correct, just slower on large links.
- lld is never chosen automatically. It remains opt-in through `LD_PREF`.

## Verification

On fedora:44 (g++ 16.2, binutils 2.46, no `ld.gold`), with this change alone on top
of master:

- Bootstrap from the d5.12.0 boot archive, then `concierge.py full-upgrade --backend c`,
  succeeded.
- A test program was linked four ways. `cm3 -commands` showed the `-fuse-ld` flag used
  each time:

| Case | `-fuse-ld` used | Result |
|---|---|---|
| Default, no gold installed | none | links and runs |
| `-DLD_PREF=" -fuse-ld=gold"` | gold | fails, as expected without gold |
| `-DLD_PREF=" -fuse-ld=bfd"` | bfd | links and runs |
| `/usr/bin/ld.gold` present (a symlink to `ld.bfd`) | gold | links and runs |

Building this on Debian 12 also needs the 128-bit EXTENDED fix for g++ < 13,
which is a separate, unrelated change.
