# OpenACC `declare` vs `data` Reproducer

This reproducer isolates the difference between:

1. routine-scope `!$acc declare copyin/copy/create(...)`
2. an explicit routine-local `!$acc data copyin/copy/create(...)` region

The benchmark is structured to match the case you described:

- `a`, `b`, `c`, and `d` are made resident on the device before the timed calls.
- the timed routines then use `copyin(a,b)`, `copy(c)`, and `copy(d)` so those clauses should hit the implicit present-or behavior rather than perform fresh transfers
- the only per-call allocations that should be unavoidable are the local scratch arrays in `create(tmp1,tmp2)`

## Files

- `repro_declare_vs_data.F90`: standalone benchmark
- `repro_many_create_3d.F90`: closer-to-`lsmruc` benchmark with 104 separate local 3D scratch arrays
- `Makefile`: build and run helper

## Build

Default flags use `-O3` and `-Mstack_arrays`, target separate-memory mode, and compile GPU code for the native GPU architecture on the build host:

```bash
make
```

That expands to:

```bash
nvfortran -O3 -Mstack_arrays -acc -gpu=ccnative,mem:separate -Minfo=accel -o repro_declare_vs_data repro_declare_vs_data.F90
```

If you want to match your environment more closely, override `GPUFLAGS`, for example:

```bash
make clean
make GPUFLAGS='-O3 -Mstack_arrays -acc -gpu=cc90,mem:separate -Minfo=accel'
```

## Run

```bash
make run
make run-many
```

Defaults:

- `n = 26214400`
- `repeats = 200`

You can override them:

```bash
./repro_declare_vs_data 104857600 100
```

With `-Mstack_arrays`, this larger `n` also means the routine-local `tmp1` and `tmp2`
automatic arrays are much larger. On many systems you will need a larger stack limit,
for example:

```bash
ulimit -s unlimited
```

## What Is Timed

For each call:

- `entry`: host time from immediately before the subroutine call to the first executable statement after the mapping setup
- `exit`: host time from immediately before routine return or `!$acc end data` to the point when the subroutine returns to the caller

The code performs one untimed warm-up call of each variant after `acc_init` and after the outer `enter data` has established device residency.

## Why This Matches The Reported Behavior

The outer region is:

```fortran
!$acc enter data copyin(a,b,c_decl,c_data) create(d_decl,d_data)
```

That ensures the inner routines see resident dummy arrays. Inside the timed routines:

- `declare` version uses routine-scope `!$acc declare copyin(a,b)`, `copy(c)`, `copy(d)`, `create(tmp1,tmp2)`
- `data` version uses one explicit `!$acc data copyin(a,b) copy(c,d) create(tmp1,tmp2)`

Both routines execute the same two OpenACC kernels and use the same local scratch arrays.

## Optional Lowering Inspection

If NVIDIA wants host-side lowering details in addition to timings:

```bash
make asm
```

This emits `repro_declare_vs_data.s`.

In a local compile with `nvfortran 26.1`, two details stood out:

- `-Minfo=accel` reported `create(tmp1,tmp2)` for the explicit `data` variant, but not for the routine-scope `declare` variant
- the generated assembly lowered the `declare create(tmp1,tmp2)` path through `pgf90_auto_alloc04_i8` plus `__pgi_uacc_mirror_allocd` and `__pgi_uacc_mirror_deallocb`, while the explicit `data create(tmp1,tmp2)` path used the normal `__pgi_uacc_dataonb` and `__pgi_uacc_dataoffb2` sequence

That does not prove the runtime cause by itself, but it is evidence that the two forms are not implemented identically.

## Closer `lsmruc` Reproducer

The first benchmark only has two local scratch arrays. The second benchmark in
`repro_many_create_3d.F90` is intentionally closer to the real `lsmruc`
structure:

- 104 separate routine-local `create(...)` arrays, matching the real routine's
  local-create object count
- 3D automatic array shapes instead of 1D vectors
- the same outer `enter data` strategy so the dummy `copy/copyin` clauses still
  hit present-or behavior
- no nested conditional data region, by request

Build and run it with:

```bash
make repro_many_create_3d
ulimit -s unlimited
./repro_many_create_3d 500 6 500 100
```

The closer reproducer now enables an untimed allocator polluter by default
between timed calls. Each pass queries current free device memory with the
OpenACC runtime API, then tries to allocate and free uneven `acc_malloc`
chunks totaling about 80% of that free memory. To disable it explicitly:

```bash
./repro_many_create_3d 500 6 500 100 0
```

The closer reproducer also enables deterministic per-call shape jitter by
default so the timed routine's local automatic arrays do not have the exact
same shape on every iteration. The current cycle perturbs `(ni,nk,nj)` across
8 states near the requested base shape. To disable both the polluter and the
jitter explicitly:

```bash
./repro_many_create_3d 500 6 500 100 0 0
```

To stress the OpenACC present table directly, add many extra long-lived
resident mappings that stay alive for the full benchmark. A good starting
point is:

```bash
./repro_many_create_3d 500 6 500 100 0 0 20000 64
```

or:

```bash
make run-many-present
```

That creates 20,000 extra resident mappings, each 64 `real` elements long,
with a small gap between them so adjacent ranges are less likely to be merged.

Arguments are:

- `ni`
- `nk`
- `nj`
- `repeats`
- optional `polluter_passes` (default `1`; set `0` to disable the untimed allocator polluter)
- optional `jitter_enable` (default `1`; set `0` to keep the timed routine shape fixed)
- optional `resident_map_count` (default `0`; number of extra long-lived resident mappings to keep alive during the benchmark)
- optional `resident_map_elems` (default `256`; length in `real` elements of each extra resident mapping)

The defaults above allocate roughly 595 MiB of routine-local 3D scratch storage
per call in the declare and data variants, before any compiler/runtime overhead.

For host-side lowering inspection of the closer reproducer:

```bash
make asm-many
```

## Suggested NVIDIA Handoff

Send:

- `repro_declare_vs_data.F90`
- `Makefile`
- optionally `repro_declare_vs_data.s` from `make asm`
- the exact compiler version from `nvfortran --version`
- GPU model and driver version
- the run command and stdout

Questions this reproducer is intended to answer:

- Why is routine exit substantially slower with routine-local `declare` than with an explicit `data` region when the dummy arrays are already present?
- If `copy/copyin` hit present-or behavior and do not transfer data, what extra runtime work is occurring on routine exit in the `declare` case?
- Is local `declare create(...)` lowered differently from `data create(...)` in a way that makes per-call teardown materially more expensive?
