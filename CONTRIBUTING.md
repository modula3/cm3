# Contributing to CM3

Contributions are welcome: bug reports, fixes, tests, documentation and new work. This page says how to send a change and what we ask of it.

## Sending a change

1. Fork `modula3/cm3` and push your branch to your fork. Only maintainers push branches to the main repository, and that is not needed to contribute.
2. Open a pull request from your branch into `modula3/cm3:master`. CI runs on pull requests, including ones from forks. Open the pull request as a draft if you want CI to run while you work; mark it "Ready for review" when you want it merged.
3. Pull requests are squash-merged, so the branch becomes one commit on master. You don't need to rebase or tidy its history yourself.

To report a bug, open an [issue](https://github.com/modula3/cm3/issues) with a small program that shows it, what you expected, and the target and version (`cm3 -version`). For questions and proposals, use [Discussions](https://github.com/modula3/cm3/discussions).

## Building and testing

[docs/INSTALL.md](docs/INSTALL.md) says how to build CM3 from this repository. After a change, rebuild and run the compiler test suite:

```sh
scripts/concierge.py upgrade            # or full-upgrade, to rebuild the libraries too
cd m3-sys/m3tests && cm3 -DHTML         # results in m3tests-results.xml
```

CI runs m3tests on every target and fails a job if a test fails that is not listed in `m3-sys/m3tests/known-failures/<TARGET>-<backend>`, or if a listed test passes. When you fix a test, remove it from the list in the same change.

A new test goes in a directory under `m3-sys/m3tests/src` (`p` tests run a program, `e` tests expect compile errors, `c` tests only compile, `r` tests end in a runtime error), with its expected output files, and is registered in `m3-sys/m3tests/src/m3makefile`. Make its output the same on 32- and 64-bit targets where you can, for example by writing values in terms of `FIRST` and `LAST`.

## Guidelines

These were agreed in [Discussion #1256](https://github.com/modula3/cm3/discussions/1256). They apply to every contribution, and in particular to work done with AI tools, which is welcome.

1. **Disclose it.** Add an `Assisted-by:` trailer to commits (some tools add `Co-Authored-By:` by default; that's fine too), or a line in the PR description.
2. **You own every line.** The submitter must understand the change well enough to explain and defend it in review without going back to the tool.
3. **Evidence, not narrative.** Bug fixes should come with a minimal reproducer, ideally a new m3tests case, and with m3tests results before and after. Where a test doesn't fit (build, CI or bootstrap changes), say what you checked and how a reviewer can see it. Keep long tool-generated analyses out of issues and PRs; summarise the cause and link to details if needed.
4. **Focused PRs.** Aim for one problem per PR. Small related fixes found along the way (comments, typos) can ride along if the description lists them. WIP commits on a branch are fine; what's proposed for merge should be reviewable as a whole.
5. **Coordinate before large efforts.** For new backends, IR changes, or runtime and ABI changes, open a discussion first so we don't build parallel versions of the same thing.
6. **Credit others' work.** If a change builds on someone else's code or patch, say so in the commit message or PR, whatever tools were involved.
7. **Merge deliberately.** Push to a branch and let CI run as you go; that's free. Merge only when CI is green, or when every failure is a known, tracked one such as #1287, with a note saying so in the PR. Changes to the compiler, the runtime (threads, GC, exceptions) or the ABI should wait for a review from someone other than the author. Small doc, CI and test-only changes don't need one.

## Licence

CM3 is open source, under the licences in [COPYRIGHTS](COPYRIGHTS) and the `COPYRIGHT-*` files.
