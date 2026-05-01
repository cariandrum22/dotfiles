# Test-Driven Development (Kent Beck Style)

## Purpose
This rule institutionalises Kent Beck’s TDD discipline (Red → Green → Refactor) across all services and libraries. It ensures new behaviour is always shaped by tests, feedback cycles remain tight, and code bases retain high design flexibility even under aggressive delivery timelines.

## Core Commitments
- **No production code without a failing test**: new behaviour starts as a failing test that expresses intent in domain language.
- **Red → Green → Refactor**: advance strictly in that order; stop as soon as the test fails (Red), write the minimal code to make it pass (Green), and then improve design without changing behaviour (Refactor).
- **One failing test at a time**: maintain a safe monotonically improving test suite; pause other work when the suite is red.
- **Baby steps**: micro-commits and micro-refactors reduce cognitive load and improve reviewability.
- **Ubiquitous language**: test names, descriptions, and Given/When/Then prose mirror the bounded context vocabulary (see `architecture-ddd-hexagonal.md`).

## Workflow
1. **Select the next behaviour**  
   - Extract from domain stories, specification by example, or failing acceptance tests.  
   - Document assumptions in the associated ADR or requirements log.
2. **Write the minimal failing test**  
   - Choose the smallest boundary that reveals the behaviour (prefer unit or use-case tests).  
   - If infrastructure is involved, begin with in-memory/fake adapters.  
   - Keep assertions single-purpose; capture the intended shape of the domain.
3. **Run the full suite**  
   - Confirm the new test fails for the expected reason.  
   - If multiple tests fail, bisect or isolate before proceeding.
4. **Make it green**  
   - Implement the simplest logic that passes just the failing test.  
   - Avoid premature abstractions or generalisation.  
   - Commit with message prefix `[RED]`, `[GREEN]`, or `[REFACTOR]` when the repository’s workflow tracks the cycle.
5. **Refactor mercilessly**  
   - Remove duplication between tests and production code (Four Rules of Simple Design).  
   - Push boundary-specific tweaks down into adapters; keep domain model pristine.  
   - Re-run the suite; only commit refactors on green.
6. **Repeat**  
   - Continue until acceptance criteria are met.  
   - Update reference documentation, ADRs, and rule indexes as needed.

## Test Suite Structure
| Layer | Goal | Examples |
|-------|------|----------|
| **Unit/Micro tests** | Exercise value objects, entities, domain services with pure collaborators. | Validate invariants, calculation rules, aggregate behaviours. |
| **Application/Use-case tests** | Verify orchestration, ports, and transactional boundaries using fakes. | Command handlers, domain events, policy enforcement. |
| **Contract/Adapter tests** | Lock down expectations between ports and infrastructure adapters. | Repository semantics, HTTP/gRPC gateways, integration events. |
| **Acceptance/End-to-end** | Demonstrate user-facing stories with realistic adapters. | HTTP endpoints, CLI flows, system-level behaviours. |

Follow the pyramid: many micro tests, fewer integration tests, scarce full-stack tests. Acceptance tests may lead TDD by writing failing scenarios before unit work begins (“outside-in”).

## Guardrails & Metrics
- **Commit hygiene**: keep RED/GREEN/REFACTOR steps atomic. If forced to branch, annotate commits with the cycle phase.
- **Mutation testing**: apply to critical domains to ensure assertions are strong. Track mutation score alongside coverage.
- **Coverage targets**: satisfy `code-coverage-policy.md`; missing lines must be justified with TODOs referencing a ticket.
- **Test latency**: suite should execute fast enough (≤ 3 min) to run on every edit cycle; extract slow tests to targeted jobs if needed.
- **Tooling**: integrate watch modes (`cargo watch`, `pytest --maxfail=1`, `vitest --watch`) to support tight cycles.

## Anti-patterns
- Writing production code first (“Greenfield Implementation”) with tests added afterwards.  
- Multiple failing tests left unattended or flaky suites left unresolved.  
- Monster tests covering multiple behaviours (assertion hoarding).  
- Refactoring on red or skipping refactor stage entirely.  
- Test names duplicating implementation rather than behaviour (“test_calculate” vs “calculates_interest_for_grace_period”).  
- Mocking everything by default instead of preferring real value objects and fakes simulated via ports.

## Collaboration Practices
- Pair/mob programming sessions follow the same RED/GREEN/REFACTOR cadence; rotate driver every test.  
- Code reviews include a checklist: Was there a failing test first? Is each commit labelled by phase? Are refactors free of behavioural changes?  
- Acceptance tests form shared living documentation; keep them readable and close to ubiquitous language.

## References
- Kent Beck, *Test-Driven Development by Example*.  
- Martin Fowler, *Mocks Aren’t Stubs*.  
- Related rules: `architecture-ddd-hexagonal.md`, `code-coverage-policy.md`, `technical-reasoning-policy.md`.
