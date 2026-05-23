## What

{Brief description of the changes}

## Why

{Why this change is needed}

## How

{Brief explanation of the implementation approach}

## Type

- [ ] Feature
- [ ] Bug Fix
- [ ] Refactor
- [ ] Docs
- [ ] Test

## Checklist

- [ ] `sbt test` passes (or the relevant `sbt "testOnly *Spec"`)
- [ ] Tests added/updated for new or changed behavior (edge cases: zero vectors, leap seconds, calendar/anomaly boundaries)
- [ ] Scaladoc (`@since`/`@throws`/params) updated for public API changes
- [ ] No existing public API removed or renamed without justification
- [ ] No hardcoded secrets or sensitive information (out of scope for this project and should be checked always)
- [ ] CI/build config (`.github/workflows/`, `build.sbt`) unchanged unless intentional (changing build spec requires a valid explaination. Avoid using libraries for maths and write in scala to reduce bundle size.)
