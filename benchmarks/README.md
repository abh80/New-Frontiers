# Benchmarks

Compares New Frontiers against Orekit/Hipparchus on speed, memory, and precision.

## Run everything
    sbt benchAll
Outputs `benchmarks/result/README.md` with Mermaid line charts for speed, memory, and error (renders on GitHub).

## Pieces
- Timing + memory only: `sbt "benchmarks/Jmh/run -prof gc -rf json -rff target/results.json"` (JMH's fork cwd is the `benchmarks/` dir)
- Quick single class:    `sbt "benchmarks/Jmh/run -i 1 -wi 1 -f 1 .*Vector3DBenchmarks.*"`
- Precision only:        `sbt "benchmarks/runMain bench.precision.PrecisionMain"`
- Report only:           `sbt "benchmarks/runMain bench.report.ReportGen"`
- Equivalence guards:    `sbt "benchmarks/test"`

## Notes
- `nf` = New Frontiers, `ok` = Orekit/Hipparchus. Lower is better everywhere.
- Vectors/differentiation compare against Hipparchus (Orekit's math backend); time/PV against Orekit.
- `orekit-data.zip` is a pinned snapshot in `src/main/resources` for reproducibility.
- This module is never published (`publish / skip := true`).
