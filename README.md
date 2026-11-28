# Critical Mass Modula-3

[Modula-3](https://modula3.github.io/cm3/reference/) is a systems programming language. It is strongly typed and memory safe, with modules and interfaces, objects, garbage collection, threads and exceptions, plus explicitly marked unsafe code for low-level work.

This repository holds **CM3** (Critical Mass Modula-3), the actively maintained implementation. It contains the `cm3` compiler, which is written in Modula-3 and builds itself, the runtime, the standard libraries and many applications.

## Getting CM3

Releases are on the [Releases](https://github.com/modula3/cm3/releases) page. See **[docs/INSTALL.md](docs/INSTALL.md)** for how to install a release on Linux, macOS or Windows, and how to build from this repository.

In short, on Linux or macOS:

```sh
# 1. Build the bootstrap compiler from a release's cm3-boot tarball.
cmake -S cm3-boot-<TARGET>-<VERSION> -B build -G Ninja -DCMAKE_INSTALL_PREFIX=$HOME/cm3
cmake --build build && cmake --install build
export PATH=$HOME/cm3/bin:$PATH

# 2. Use it to build and install the full system from this repository.
git clone https://github.com/modula3/cm3 && cd cm3
scripts/concierge.py full-upgrade
```

## Supported targets

Continuous integration builds these targets on every change, and runs the compiler test suite on all of them except `AMD64_MINGW`:

| Target | Platform |
|---|---|
| `AMD64_LINUX` | Linux on x86-64 |
| `I386_LINUX` | Linux on 32-bit x86 |
| `ARM64_DARWIN` | macOS on Apple silicon |
| `AMD64_DARWIN` | macOS on Intel |
| `AMD64_NT` | 64-bit Windows, Visual C++ |
| `I386_NT` | 32-bit Windows, Visual C++ |
| `AMD64_MINGW` | 64-bit Windows, MinGW |

Configurations for many other systems are in [`m3-sys/cminstall/src/config`](m3-sys/cminstall/src/config), but they are not regularly tested.

## Documentation

- [The language definition](https://modula3.github.io/cm3/reference/)
- [A tutorial](https://modula3.github.io/cm3/tutorial/m3/m3_toc.html)
- [Library interfaces](https://modula3.github.io/cm3/help/interfaces.html)
- [Books and papers](https://modula3.github.io/cm3/help/bib.html)
- [`examples/`](examples) holds small example programs.

This documentation dates from earlier releases. A new site is being built: see [#1434](https://github.com/modula3/cm3/issues/1434).

## Repository layout

- `m3-sys/`: the compiler: front end, back ends, linker, the Quake build language and the `cm3` driver, plus `cminstall/src/config` (per-target configurations) and `m3tests` (the compiler test suite).
- `m3-libs/`: the runtime (`m3core`), the standard library (`libm3`) and other libraries.
- `m3-ui/`, `m3-comm/`, `m3-db/`, `m3-www/`, `m3-tools/` and the other `m3-*` directories: optional libraries and applications.
- `scripts/`: build scripts. `concierge.py` is the main one, and `scripts/pkginfo.txt` lists the packages in build order.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for how to send a change and what we ask of it. Report bugs and propose changes through [issues](https://github.com/modula3/cm3/issues) and [pull requests](https://github.com/modula3/cm3/pulls). To run the compiler test suite after building:

```sh
cd m3-sys/m3tests && cm3 -DHTML
```

## Licence

CM3 is open source. Different parts are under different licences: see [`COPYRIGHTS`](COPYRIGHTS) and the `COPYRIGHT-*` files in this directory.
