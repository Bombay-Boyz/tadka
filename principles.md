# Haskell Principles

 Code developed should read like mathematical proof. It should be idiomatic, algorithmic       and use ADTs, GADTs, smart constructors and linear types wherever possible.
  Use pure functions and isolate side effects.
  Make illegal states unrepresentable with types.
  Use explicit, descriptive types and signatures.
  No partial functions to be used.
  Keep functions small, composable, and single-purpose.
  Prefer immutable data.
  Model domains with algebraic data types (data, newtype, type).
  Use newtype for type safety with zero runtime cost.
  Minimise unnecessary type class constraints.
  Prefer concrete types at module boundaries.
  Leverage parametric polymorphism where appropriate.
  Use standard abstractions (Functor, Applicative, Monad, Foldable, Traversable) idiomatically.
  Prefer Applicative unless Monad is required.
  Avoid orphan instances.
  Keep modules cohesive with clear responsibilities and strong boundaries.
  Hide implementation details via explicit export lists.
  Organise code by domain rather than utility.
  Minimise global state and mutable references.
  Handle errors explicitly (Maybe, Either, custom error types). Never allow silent failures.
  Write property-based tests alongside unit tests.
  Enable compiler warnings and treat them as errors.
  Use formatting and linting tools consistently.
  Document public APIs and non-obvious decisions.
  Keep dependencies minimal and well maintained.
  Follow consistent naming conventions.
  Write deterministic, reproducible builds.
  Maintain referential transparency wherever possible.
  Exploit laziness intentionally; avoid accidental space leaks.
  Use strictness annotations only when justified by profiling.
  Keep IO at the edges of the application.
  Separate business logic from infrastructure.
  Use package and module versioning responsibly.
  Ensure code is portable
  Prefer existing, well-tested libraries over custom implementations.
  Preserve backward compatibility where practical.
  Continuously review and simplify type and module design.
