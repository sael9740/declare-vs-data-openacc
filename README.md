# OpenACC Present-Table Reproducer

This is a fixed reproducer for the `declare` vs `data` exit-time difference.

It compares:

- routine-local `!$acc declare copyin/copy/create(...)`
- an explicit routine-local `!$acc data copyin/copy/create(...)`

The dummy arrays are made resident before the timed calls with an outer
`enter data`, and the program also creates 20,000 extra long-lived resident
mappings to stress the OpenACC present table.

Fixed configuration:

- `ni=500`
- `nk=6`
- `nj=500`
- `repeats=100`
- `resident_map_count=20000`
- `resident_map_elems=64`

Build:

```bash
make
```

Run:

```bash
ulimit -s unlimited
./repro_many_create_3d
```

Help:

```bash
./repro_many_create_3d --help
```

You can override any parameter with named options, for example:

```bash
./repro_many_create_3d --ni 500 --nk 6 --nj 500 --repeats 100 \
  --resident-map-count 20000 --resident-map-elems 64 --resident-map-gap 17
```

Optional host-side lowering:

```bash
make asm
```
