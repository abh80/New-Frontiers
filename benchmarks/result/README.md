# New Frontiers vs Orekit/Hipparchus benchmark baseline

Lower is better for speed and memory; higher is better for precision. Every chart has two
lines, New Frontiers first and Orekit/Hipparchus second. Speed (ns/op) and memory (bytes/op)
come from JMH. Precision is shown as accurate decimal digits, `-log10(absolute error)`, capped
at 18; a point at 18 means the result was exact. Regenerate with `sbt benchAll`.

## Differentiator

### Performance and memory

```mermaid
xychart-beta
    title "Differentiator speed (ns/op, lower is better)"
    x-axis ["firstDerivative"]
    y-axis "ns/op"
    line [228.5]
    line [306.2]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

```mermaid
xychart-beta
    title "Differentiator memory (bytes/op, lower is better)"
    x-axis ["firstDerivative"]
    y-axis "bytes/op"
    line [736]
    line [1088]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

### Precision

```mermaid
xychart-beta
    title "Differentiator precision (accurate digits, higher is better)"
    x-axis ["exp", "recip", "sin"]
    y-axis "digits"
    line [12.93, 12.98, 14.14]
    line [12.93, 12.98, 14.14]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

## Kinematic

### Performance and memory

```mermaid
xychart-beta
    title "Kinematic speed (ns/op, lower is better)"
    x-axis ["momentum", "shift"]
    y-axis "ns/op"
    line [4.683, 11.19]
    line [10.59, 24.42]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

```mermaid
xychart-beta
    title "Kinematic memory (bytes/op, lower is better)"
    x-axis ["momentum", "shift"]
    y-axis "bytes/op"
    line [40, 104]
    line [40, 104]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

### Precision

```mermaid
xychart-beta
    title "Kinematic precision (accurate digits, higher is better)"
    x-axis ["propagate_120s"]
    y-axis "digits"
    line [18]
    line [14.45]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

## Time

### Performance and memory

```mermaid
xychart-beta
    title "Time speed (ns/op, lower is better)"
    x-axis ["construct", "durationFrom", "shift", "utcOffset"]
    y-axis "ns/op"
    line [72.07, 3.801, 6.231, 20.35]
    line [11.35, 0.9353, 3.995, 15.79]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

```mermaid
xychart-beta
    title "Time memory (bytes/op, lower is better)"
    x-axis ["construct", "durationFrom", "shift", "utcOffset"]
    y-axis "bytes/op"
    line [368, 0, 48, 56]
    line [32, 0, 32, 0]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

### Precision

```mermaid
xychart-beta
    title "Time precision (accurate digits, higher is better)"
    x-axis ["accumulate_0.1s_x10M"]
    y-axis "digits"
    line [18]
    line [10]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

## Vector3D

### Performance and memory

```mermaid
xychart-beta
    title "Vector3D speed (ns/op, lower is better)"
    x-axis ["add", "angleTo", "cross", "dot", "magnitude", "normalize", "rotateX", "scale", "sub"]
    y-axis "ns/op"
    line [4.858, 10.42, 4.496, 1.019, 1.626, 4.979, 13.08, 5.957, 5.605]
    line [8.373, 90.87, 36.09, 9.853, 2.648, 6.442, 27.12, 5.559, 6.763]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

```mermaid
xychart-beta
    title "Vector3D memory (bytes/op, lower is better)"
    x-axis ["add", "angleTo", "cross", "dot", "magnitude", "normalize", "rotateX", "scale", "sub"]
    y-axis "bytes/op"
    line [40, 0, 40, 0, 0, 40, 40, 40, 40]
    line [40, 0, 40, 0, 0, 40, 72, 40, 40]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

### Precision

```mermaid
xychart-beta
    title "Vector3D precision (accurate digits, higher is better)"
    x-axis ["cross", "normalize", "rotateX"]
    y-axis "digits"
    line [18, 18, 15]
    line [18, 18, 14.9]
```
*First line: New Frontiers. Second line: Orekit/Hipparchus.*

