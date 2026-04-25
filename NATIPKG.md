# pyffi natipkg integration

This document explains the platform-specific natipkg companions
that `pyffi-lib` declares as build-dependencies, and how they fit
into the catalogue / build / install flow. It is reference
material for maintainers; end users do not need to read this.

## What problem this solves

The Racket package-build server at `pkg-build.racket-lang.org`
runs on a Linux VM that has no Python development files
installed. Building `pyffi-doc` requires evaluating Scribble
example blocks that import real Python through pyffi's FFI.
Without a `libpython.so` reachable on disk, those evaluations
abort and the catalogue never produces a successful pyffi build
artefact — which has been pyffi's state on the catalogue since
2022 (1254 days as of 2026-04-25, with `success-log = #f` on every
package metadata snapshot).

The fix is the standard convention used by `math-lib`,
`draw-lib`, `gui-lib`, etc.: ship the platform-specific binary
through a sibling natipkg package, declared as a
platform-conditional build-dep.

## The four natipkg companions

| Catalogue package                | Platform regex                  | Source                                 |
| :------------------------------- | :------------------------------ | :------------------------------------- |
| `pyffi-aarch64-linux-natipkg`    | `aarch64-linux`                 | python-build-standalone install_only_stripped |
| `pyffi-x86_64-linux-natipkg`     | `x86_64-linux-natipkg`          | python-build-standalone install_only_stripped |
| `pyffi-aarch64-macosx-natipkg`   | `aarch64-macosx`                | python-build-standalone install_only          |
| `pyffi-x86_64-macosx-natipkg`    | `x86_64-macosx`                 | python-build-standalone install_only          |

The build server identifies as `x86_64-linux-natipkg`
specifically (distinct from plain `x86_64-linux`), so the second
entry is the one that fires there. The other three exist so a
maintainer running `raco setup` on another host (e.g. building
docs locally before publishing) gets the same tooling without
having to install Python.

All four bundle CPython 3.12 from
[`astral-sh/python-build-standalone`](https://github.com/astral-sh/python-build-standalone).
Their source repository is
[`lamestllama/pyffi-natipkg-bundle`](https://github.com/lamestllama/pyffi-natipkg-bundle)
— one git tree with four subdirs, one catalogue package per
subdir.

## info.rkt

In `pyffi-lib/info.rkt`:

```racket
(define deps '("base" "at-exp-lib"))   ; runtime — unchanged

(define build-deps
  '("base" "at-exp-lib"
    ("pyffi-aarch64-linux-natipkg"  #:platform "aarch64-linux")
    ("pyffi-x86_64-linux-natipkg"   #:platform "x86_64-linux-natipkg")
    ("pyffi-aarch64-macosx-natipkg" #:platform "aarch64-macosx")
    ("pyffi-x86_64-macosx-natipkg"  #:platform "x86_64-macosx")))
```

Notice `build-deps`, not `deps`. End users running
`raco pkg install pyffi` against the source catalogue or pulling
the prebuilt snapshot do not download a natipkg — only build
hosts (running `raco setup` for compilation/doc-rendering) do.
That keeps a normal install ~50 MB lighter than it would
otherwise be.

## Discovery logic

`pyffi-lib/pyffi/libpython.rkt` resolves the libpython path in
this order, returning the first hit:

1. `PYFFI_LIBPYTHON` environment variable
2. `pyffi:libdir` / `pyffi:home` preferences (set by
   `raco pyffi configure`)
3. The matching natipkg companion (located via `pkg-directory`
   from `pkg/lib`; returns `#f` cleanly if the package is not
   installed)
4. Dynamic loader search by candidate name (`libpython3.12`,
   `libpython3`, etc.)
5. Error

`pyffi-lib/pyffi/python-initialization.rkt` parallels this for
PYTHONHOME — it falls through to the natipkg's package root
(canonicalised via `simple-form-path`) if no `pyffi:home` /
`pyffi:data` preference is set.

The user-facing precedence is unchanged from before: explicit
configuration always wins over auto-discovery. The natipkg path
is purely a fallback, only fires in build environments where it's
been installed via build-deps.

## Catalogue registration

For pkg-build to actually use the natipkgs, the four packages
need entries on `pkgs.racket-lang.org`. Each entry's source URL
is the bundle repo with a `?path=` query string:

```
pyffi-aarch64-linux-natipkg
  → https://github.com/lamestllama/pyffi-natipkg-bundle.git?path=pyffi-aarch64-linux-natipkg#main

pyffi-x86_64-linux-natipkg
  → https://github.com/lamestllama/pyffi-natipkg-bundle.git?path=pyffi-x86_64-linux-natipkg#main

pyffi-aarch64-macosx-natipkg
  → https://github.com/lamestllama/pyffi-natipkg-bundle.git?path=pyffi-aarch64-macosx-natipkg#main

pyffi-x86_64-macosx-natipkg
  → https://github.com/lamestllama/pyffi-natipkg-bundle.git?path=pyffi-x86_64-macosx-natipkg#main
```

Each is its own catalogue entry, separately versioned by
checksum. Once registered, the next pkg-build run picks them up
through pyffi-lib's `build-deps` and finally produces a successful
pyffi build.

The bundle repo can stay under `lamestllama` or be transferred to
`soegaard` at the maintainer's discretion — the catalogue source
URLs change in either case but nothing in pyffi-lib needs to
move.

## Updating the bundled Python

When a new python-build-standalone release is worth picking up:

1. Choose the new release date (e.g. `20260615`) and Python
   patch version (e.g. `3.12.14`).
2. In `pyffi-natipkg-bundle`, replace each subdir's `lib/`
   contents from the matching install_only(_stripped) tarball.
   Keep the Linux symlinks
   (`libpython3.12.so → libpython3.12.so.1.0`, `libpython3.so`).
3. Update each `info.rkt`'s `pkg-desc` if the Python version
   string in it changed.
4. Commit all four updates together (single commit, `git push`).
5. The catalogue's daily refresh picks up the new checksum;
   pkg-build will rebuild pyffi-lib + pyffi-doc against the new
   bundled Python.

A version bump is one PR to one repo, four file replacements.

## End-user perspectives

| Scenario                                                                | Files downloaded                                          | Need libpython at install? | Need libpython at runtime?       |
| :---------------------------------------------------------------------- | :-------------------------------------------------------- | :------------------------- | :------------------------------- |
| `raco pkg install pyffi` against snapshot catalogue (prebuilt)          | pyffi-lib + pyffi-doc only (with prebuilt .zo + HTML)     | No (artefacts prebuilt)    | Yes, via configured Python       |
| `raco pkg install pyffi` against source catalogue                       | pyffi-lib + pyffi-doc + matching natipkg (build-dep)      | Yes (uses natipkg)         | Yes, via configured Python       |
| `raco pkg install pyffi --no-setup`                                     | pyffi-lib only (no doc-build, no natipkg)                 | No                         | Yes, via configured Python       |
| `raco pkg install pyffi-<arch>-<os>-natipkg` directly                   | one natipkg                                               | n/a                        | Bundled Python via natipkg path  |

All paths converge on the user having a usable Python at
runtime; the natipkg only ever supplies `libpython` to the
build stage.

## Files in this PR

- `pyffi-lib/info.rkt` — adds the four `build-deps` entries.
- `pyffi-lib/pyffi/libpython.rkt` — adds natipkg discovery
  (`current-natipkg-name`, `natipkg-lib-dir`, `pyffi-natipkg-root`)
  in the resolver between user preferences and the loader search.
- `pyffi-lib/pyffi/python-initialization.rkt` — falls through to
  the natipkg root for PYTHONHOME; canonicalises via
  `simple-form-path`; uses `(or (get-preference …) "default")`
  for `pyver` / `platlibdir` reads so a preference deliberately
  cleared to `#f` doesn't crash Python at module init.
- `NATIPKG.md` — this document.

## What this PR does not change

- The user-facing API of pyffi.
- The `raco pyffi configure` / `raco pyffi show` flow.
- The behaviour for users with a configured system Python (their
  configuration still wins in step 2 of the discovery order; the
  natipkg path never overrides it).
- Existing CI workflows or test runs.
