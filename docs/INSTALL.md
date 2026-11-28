# Installing CM3

CM3's compiler is written in Modula-3, so you need a Modula-3 compiler to build it. Each release provides one of two things:

- **A ready-made package** for a few systems: a Debian package and a Windows installer. If one fits your system, it is the quickest route.
- **A bootstrap tarball** for each supported target. It holds the compiler pre-translated to C++. You build it with CMake, then use it to build the full system from this repository's sources.

All downloads are on the [Releases](https://github.com/modula3/cm3/releases) page. The examples below use release `d5.12.0`. Substitute the latest release.

## Ready-made packages

### Debian and Ubuntu (x86-64)

`cm3_5.12.0_amd64.deb` installs a complete system under `/usr/local/cm3`.

```sh
sudo apt install ./cm3_5.12.0_amd64.deb
export PATH=/usr/local/cm3/bin:$PATH
```

It needs glibc 2.40 or later (Debian 13, Ubuntu 25.04 or later). On older systems, use the bootstrap route below.

### Windows

`cm3_win32_5.12.0_setup.exe` is a Windows installer. To build from the latest sources instead, use the bootstrap route below.

## Building from a bootstrap tarball

This takes three steps:

1. Build the bootstrap compiler with CMake.
2. Use it to build the compiler and libraries from a clone of this repository.
3. Put the result on your `PATH`.

The full build takes from a few minutes to half an hour, depending on the machine.

Pick the tarball for your target:

| Target | Platform | Bootstrap file |
|---|---|---|
| `AMD64_LINUX` | Linux on x86-64 | `cm3-boot-AMD64_LINUX-d5.12.0.tar.xz` |
| `I386_LINUX` | Linux on 32-bit x86 | `cm3-boot-I386_LINUX-d5.12.0.tar.xz` |
| `ARM64_DARWIN` | macOS on Apple silicon | `cm3-boot-ARM64_DARWIN-d5.12.0.tar.xz` |
| `AMD64_DARWIN` | macOS on Intel | `cm3-boot-AMD64_DARWIN-d5.12.0.tar.xz` |
| `AMD64_NT` | 64-bit Windows | `cm3-boot-AMD64_NT-d5.12.0.7z` |
| `I386_NT` | 32-bit Windows | `cm3-boot-I386_NT-d5.12.0.7z` |

For other systems, start from the tarball for the most similar target. Then name your target when building, for example `scripts/concierge.py full-upgrade --target ARM64_LINUX`. The targets CM3 knows are the file names in [`m3-sys/cminstall/src/config`](../m3-sys/cminstall/src/config).

### Prerequisites

All systems need Python 3, CMake, Git and a C/C++ compiler. Ninja is optional but faster.

**Linux (Debian, Ubuntu):**

```sh
sudo apt-get install build-essential cmake ninja-build python3 git libunwind-dev
```

`libunwind-dev` is needed on `AMD64_LINUX`. For `I386_LINUX` on a 64-bit system, also install `g++-multilib`.

**macOS:** install the Xcode command-line tools (`xcode-select --install`) and, with [Homebrew](https://brew.sh/):

```sh
brew install cmake ninja
```

Optionally, `brew install libiodbc` as well, for the ODBC database library. Without it, that library is skipped.

**Windows:** install Visual Studio (the Community edition is fine) with the C++ workload, plus [Python 3](https://www.python.org/downloads/windows/), [CMake](https://cmake.org/download/), [Git](https://git-scm.com/download/win) and [7-Zip](https://www.7-zip.org/). Run every command below from the **x64 Native Tools Command Prompt** (for `AMD64_NT`) or the **x86 Native Tools Command Prompt** (for `I386_NT`).

### Step 1: build the bootstrap compiler

Linux and macOS (here for `ARM64_DARWIN`, installing into `$HOME/cm3`):

```sh
curl -LO https://github.com/modula3/cm3/releases/download/d5.12.0/cm3-boot-ARM64_DARWIN-d5.12.0.tar.xz
tar -xJf cm3-boot-ARM64_DARWIN-d5.12.0.tar.xz          # creates bootstrap/
cmake -S bootstrap -B build -G Ninja -DCMAKE_INSTALL_PREFIX=$HOME/cm3
cmake --build build
cmake --install build
export PATH=$HOME/cm3/bin:$PATH
```

Windows (here for `AMD64_NT`, installing into `C:\cm3`; for `I386_NT` use `-A Win32`):

```bat
curl -LO https://github.com/modula3/cm3/releases/download/d5.12.0/cm3-boot-AMD64_NT-d5.12.0.7z
7z x cm3-boot-AMD64_NT-d5.12.0.7z
cmake -S bootstrap -B build -A x64 -DCMAKE_INSTALL_PREFIX=C:\cm3
cmake --build build --config Release
cmake --install build --config Release
set PATH=C:\cm3\bin;%PATH%
```

This installs only the compiler binary into `bin/`. There is no configuration file and there are no libraries yet, so `cm3` cannot compile programs until step 2 is done. Running `cm3 -version` works and is a useful check that `PATH` is right. The d5.12.0 bootstrap reports itself as `d5.11.9`, which is expected.

### Step 2: build the full system

Clone this repository and run `full-upgrade` with the bootstrap compiler on your `PATH`:

```sh
git clone https://github.com/modula3/cm3
cd cm3
scripts/concierge.py full-upgrade          # Windows: python3 scripts\concierge.py full-upgrade
```

`full-upgrade` rebuilds the compiler with the bootstrap compiler, then builds and installs the libraries with the new compiler. Everything goes into the installation that the `cm3` on your `PATH` belongs to (`$HOME/cm3` above). It writes `bin/cm3.cfg` there as well.

By default it builds the **headless** package set, which has no graphical libraries. For the full system, including the Trestle UI toolkit and the X11 applications, run `scripts/concierge.py full-upgrade all`. That needs the X11 and OpenGL development libraries:

- Debian and Ubuntu: `sudo apt-get install xorg-dev libglu1-mesa-dev`
- macOS: `brew install xquartz libxaw libxft`

To leave packages out, subtract them, for example `full-upgrade all -m3-scheme`.

### Step 3: try it

```sh
mkdir -p hello/src && cd hello/src
cat > Main.m3 <<'EOF'
MODULE Main;
IMPORT IO;
BEGIN
  IO.Put ("Hello, world!\n");
END Main.
EOF
cat > m3makefile <<'EOF'
import ("libm3")
implementation ("Main")
program ("hello")
EOF
cm3
./ARM64_DARWIN/hello          # the directory is named after your target
```

## Keeping up to date

With CM3 installed, update to the latest sources by pulling and rerunning `full-upgrade`:

```sh
cd cm3 && git pull && scripts/concierge.py full-upgrade
```

`full-upgrade` replaces the installed compiler in place. To keep a known-good installation, copy it first, for example `cp -R $HOME/cm3 $HOME/cm3.bak`.

## When something goes wrong

- **"unable to locate configuration file, cm3.cfg"**: only step 1 has been done. Run step 2.
- **The wrong target is detected**: pass `--target <TARGET>` to `concierge.py`.

For anything else, please [open an issue](https://github.com/modula3/cm3/issues) and include the last part of the build output.
