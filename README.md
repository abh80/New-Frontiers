# New Frontiers

A high-precision **astrodynamics and orbital-mechanics library for Scala 3**.

New Frontiers provides the foundational building blocks for spaceflight and astronomy
computations — astronomical time scales with attosecond resolution, 3D vector and kinematic
math, numerical differentiation, typed physical units, reference frames, and orbit models.
The design closely mirrors [Orekit](https://www.orekit.org/) concepts (`TimeScale`,
`AbsoluteTime`, `KinematicState`, `Frame`) but reimplements them idiomatically in Scala 3.

> **A note on benchmarks.** We try to be open about how New Frontiers compares to
> [Orekit](https://www.orekit.org/) and its Hipparchus math backend. There's a small benchmark
> suite that looks at speed, memory, and accuracy, and we keep the results (with charts) in the repo
> at [`benchmarks/result/`](benchmarks/result/README.md) if you'd like to take a look. It's still
> early and we add to it as the library grows, so please treat it as a work in progress rather than
> the final word. If you want to check the numbers yourself, `sbt benchAll` regenerates everything.

---

## Table of contents

- [Why New Frontiers](#why-new-frontiers)
- [Design principles](#design-principles)
- [What's available today](#whats-available-today)
- [Getting started](#getting-started)
- [Usage examples](#usage-examples)
- [Project structure](#project-structure)
- [Building & testing](#building--testing)
- [Documentation](#documentation)
- [Roadmap](#roadmap)
- [Contributing](#contributing)
- [License](#license)

---

## Why New Frontiers

Astrodynamics code lives or dies by the correctness of its **units, formulas, and time
conversions**. A degree slipped into a radian formula, or a leap second mishandled, silently
corrupts every downstream result. New Frontiers leans on Scala 3's type system to keep those
mistakes out: angles and distances are typed, time carries attosecond precision, and every
conversion routes through a single well-defined reference (TAI for time).

This is a pure numerical library. There is no UI, server, database, or network layer — the
published artifact is the `org.abh80.nf.*` API and its Scaladoc.

## Design principles

- **Correctness first.** This is a scientific library; the formulas, units, and time
  conversions *are* the product.
- **Zero runtime dependencies.** The library depends only on the Scala standard library and
  the JDK. Math is written by hand in Scala rather than pulled from external libraries, to keep
  the artifact small and the behavior fully under our control.
- **Immutable by default.** Public types are immutable; operations return new instances.
- **Typed units, SI internally.** Angles flow through `AngleUnit`, distances through
  `DistanceUnit`. All formulas operate in **radians, meters, and seconds**, converting only at
  the boundary.
- **TAI is the time hub.** Every time scale defines its offset relative to International Atomic
  Time; conversions never hard-code a direct offset between two non-TAI scales.

## What's available today

| Module | Responsibility | Status |
|--------|----------------|--------|
| `core` | Library-wide constants | ✅ Implemented |
| `core.math` | `Vector3D`, `KinematicState` (3D vectors; position/velocity/acceleration) | ✅ Implemented |
| `core.math.calculus` | Numerical differentiation (`FiniteDifferencesDifferentiator`, `Derivative`) | ✅ Implemented |
| `core.metrics` | Typed physical units (`AngleUnit`, `DistanceUnit`) | ✅ Implemented |
| `core.time` | Time scales (TAI / UTC / TT / TDB / GPS / GLONASS / IRNSS), `AbsoluteTime`, `Date`, `TimeFormat` | ✅ Implemented |
| `util` | `TimeShiftable` type-class, `DateUtil` (calendar enums & formatters) | ✅ Implemented |
| `frames` | Reference-frame tree + transforms | 🚧 Stub |
| `orbit` | `Orbit`, `KeplerianOrbit`, `CircularOrbit` | 🚧 Stub |

Highlights:

- **Attosecond-precision time** (`TimeFormat`: seconds + attoseconds, range `0..10¹⁸`).
- **Seven astronomical time scales** with leap-second handling, all converging through TAI.
- **Calendar awareness** — `Date` auto-switches Gregorian / Julian / Proleptic-Julian around the
  1582 reform.
- **3D vector algebra** — dot, cross, rotations, normalization, angle, distance.
- **Kinematic propagation** under constant acceleration.
- **Numerical differentiation** of arbitrary order via Newton divided differences.

## Getting started

### Requirements

- **JDK 11 or newer** (CI builds on JDK 11; the library relies on `java.time`).
- **sbt 1.11.x** (an `sbt` launcher; the project pins sbt `1.11.3`).
- **Scala 3.3.6** — managed automatically by sbt.

### Using the library

New Frontiers is published to **Maven Central**. Add it with sbt:

```scala
libraryDependencies += "io.github.abh80" %% "new-frontiers" % "0.1.0"
```

<details>
<summary>Gradle / Maven</summary>

**Gradle** (Kotlin DSL) — note the explicit `_3` Scala suffix:

```kotlin
implementation("io.github.abh80:new-frontiers_3:0.1.0")
```

**Maven**:

```xml
<dependency>
  <groupId>io.github.abh80</groupId>
  <artifactId>new-frontiers_3</artifactId>
  <version>0.1.0</version>
</dependency>
```
</details>

Or explore it in a REPL with the project on the classpath:

```powershell
sbt console
```

## Usage examples

> All public types live under the `org.abh80.nf` package prefix. Arithmetic operators
> (`+ - * / dot X`) are provided through companion-object operator classes — importing the
> companion (e.g. `import Vector3D.*`) brings them into scope.

### Time scales and absolute time

```scala
import org.abh80.nf.core.time.{AbsoluteTime, EpochFactory, TimeScaleFactory}

val utc = TimeScaleFactory.getUTC
val tai = TimeScaleFactory.getTAI

// The J2000 epoch: 2000-01-01 12:00 TT (noon, not midnight).
val j2000 = EpochFactory.J2000_0

// Build an absolute time in UTC, then shift it forward by one hour.
val launch = new AbsoluteTime(2024, 1, 1, 0, 0, 0.0, utc)
val later  = launch ++ 3600.0          // TimeShiftable: shift by seconds

println(later.toString)                // ISO-8601 in UTC, e.g. 2024-01-01T01:00:00Z

// Offset between two scales at this instant (UTC vs TAI = leap seconds).
val offset = launch.offsetBetween(utc, tai)
```

### 3D vectors

```scala
import org.abh80.nf.core.math.Vector3D
import org.abh80.nf.core.math.Vector3D.*   // operators: + - * / dot X ~

val a = Vector3D(1, 2, 3)
val b = Vector3D(4, 5, 6)

val sum     = a + b                 // Vector3D(5, 7, 9)
val dot     = a dot b               // 32.0
val cross   = a X b                 // Vector3D(-3, 6, -3)
val unit    = a.normalize
val angle   = a.angleTo(b)()        // AngleUnit.Radian by default
```

### Typed units

```scala
import org.abh80.nf.core.metrics.AngleUnit.*
import org.abh80.nf.core.metrics.DistanceUnit.*

val rad  = Degree(90).toRadians           // ~1.5708
val back = Degree.fromRadians(rad)        // Degree(90.0)

val meters = Kilometer(7000).toMeter      // 7_000_000.0
val au     = AstronomicalUnit(1).toMeter  // 1.495978707e11
```

### Kinematic state

```scala
import org.abh80.nf.core.math.{KinematicState, Vector3D}

val state = KinematicState(
  position     = Vector3D(7_000_000, 0, 0),  // meters
  velocity     = Vector3D(0, 7_500, 0),      // m/s
  acceleration = Vector3D.Zero               // m/s²
)

val propagated = state ++ 60.0               // shift by 60 s (constant-acceleration kinematics)
val momentum   = state.getAngularMomentum    // r × v
```

### Numerical differentiation

```scala
import org.abh80.nf.core.math.calculus.FiniteDifferencesDifferentiator

val diff = new FiniteDifferencesDifferentiator(nbPoints = 5, stepSize = 1e-3)
val d    = diff.differentiate(x => x * x)    // Double => (Int => Double)

val firstAt3  = d(3.0)(1)                     // ≈ 6.0   (f'(x)  = 2x)
val secondAt3 = d(3.0)(2)                     // ≈ 2.0   (f''(x) = 2)
```

## Project structure

```
new-frontiers/
├── build.sbt                       # Root project "New Frontiers", Scala 3.3.6
├── project/                        # sbt build config & plugins
├── src/
│   ├── main/scala/                 # package prefix org.abh80.nf
│   │   ├── core/
│   │   │   ├── Constants.scala
│   │   │   ├── math/               # Vector3D, KinematicState
│   │   │   │   └── calculus/       # Derivative, FiniteDifferencesDifferentiator
│   │   │   ├── metrics/            # AngleUnit, DistanceUnit
│   │   │   └── time/               # time scales, AbsoluteTime, Date, TimeFormat
│   │   ├── frames/                 # reference frames (stub)
│   │   ├── orbit/                  # orbit models (stub)
│   │   └── util/                   # TimeShiftable, DateUtil
│   └── test/scala/                 # *Spec.scala mirroring the main layout
└── docs/                           # architecture & per-module deep-dive docs
```

## Building & testing

All commands run through sbt (PowerShell on Windows):

```powershell
sbt compile                      # Compile the main sources
sbt test                         # Run the full test suite
sbt "testOnly *Vector3DSpec"     # Run a single spec
sbt "testOnly *.calculus.*"      # Run a package's specs
sbt doc                          # Generate Scaladoc (target/scala-3.*/api)
sbt +test                        # Cross-version test (what CI runs)
sbt console                      # REPL with the project on the classpath
```

Tests use **ScalaTest 3.2.19**. A custom `AlmostEqualsMatcher` (under
`src/test/scala/matchers/`) provides tolerance-based floating-point assertions for numerical
results.

## Documentation

- **Scaladoc** is the published API reference, generated with `sbt doc` and deployed to GitHub
  Pages via CI.
- **In-repo design docs** live in [`docs/`](docs/):
  - [`docs/architecture.md`](docs/architecture.md) — system layering and data flows
  - [`docs/tech-stack.md`](docs/tech-stack.md) — stack and coding standards
  - [`docs/modules/`](docs/modules/) — a deep-dive per module (time, math, calculus, metrics,
    frames, orbit, util)

## Roadmap

New Frontiers is being built bottom-up. The foundation (time, math, units) is in place; the next
phases add the layers that sit on top of it.

**Near term — reference frames (`frames`)**
- A working `Frame` tree rooted at an inertial frame (GCRF).
- Static and time-dependent transforms between frames; transform composition along the tree.
- Earth-fixed frames (ITRF / ECEF) and the inertial ↔ rotating conversion.

**Near term — orbit models (`orbit`)**
- `KeplerianOrbit` and `CircularOrbit` with the classical elements.
- Anomaly conversions (mean ↔ eccentric ↔ true) with an iterative Kepler solver.
- Conversion between orbital elements and `KinematicState` (position/velocity).
- Two-body (Keplerian) propagation via `TimeShiftable`.

**Medium term**
- Additional time references: UT1, sidereal time (GST/LST), and Earth Orientation Parameter
  loading.
- Equinoctial elements for near-singular orbits.
- Numerical propagators (fixed- and adaptive-step integrators) — written in Scala, no external
  math libraries.

**Longer term**
- Force models: J2 and higher-order gravity, atmospheric drag.
- Input/output: TLE parsing and common ephemeris formats.
- Publication of a stable, versioned artifact to Maven Central.

> Roadmap items are aspirational and may change. The `frames` and `orbit` packages currently
> contain placeholders only — do not depend on their runtime behavior yet.

## Contributing

Contributions are welcome. Please read the
[**Contributing Guidelines**](../../wiki/Contributing-Guidelines) in the project wiki — they spell
out the coding conventions, testing requirements, commit format, and pull-request process in full.

In short: immutable types, typed units, formulas in radians/meters/seconds, no external math
dependencies, and a spec for every new public behavior (covering the domain's edge cases — zero
vectors, leap seconds, calendar transitions, anomaly singularities, step-size sensitivity). Run a
single spec with `sbt "testOnly *YourSpec"` while iterating, and keep Scaladoc accurate — it is the
published API surface.

Issue and pull-request templates are provided under [`.github/`](.github/).

## License

Licensed under the **Nokia Open Source License (NOKOS License) Version 1.0a**
(SPDX identifier: [`Nokia`](https://spdx.org/licenses/Nokia.html)). See the [`LICENSE`](LICENSE)
file for the full terms.

---

Built by [@abh80](https://github.com/abh80).
