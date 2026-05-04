# Domain-Driven Design with Clean/Hexagonal Architecture

## Purpose
This rule codifies how we structure services and libraries that rely on Domain-Driven Design (DDD) while applying Clean Architecture and Hexagonal (Ports & Adapters) principles. It supplements the communication, reasoning, and tooling policies by standardising how we model complex domains, isolate infrastructure, and guide collaborative work on large-scale or safety-critical software.

## Core Principles
- **Ubiquitous Language First**: Every public type, module, and commit message reflects the vocabulary agreed with domain experts. Vocabulary drift is treated as a defect.
- **Bounded Contexts**: Each context owns its models, invariants, and persistence schema. Sharing of domain types across contexts is prohibited; instead, use translation or published language contracts.
- **Dependency Rule**: Flow of dependencies always points inward (Drivers → Application → Domain). The Domain layer is oblivious to frameworks, databases, or transport protocols.
- **Ports and Adapters**: Interactions with the outside world happen only through interfaces (ports) defined in the domain or application layer and implemented by adapters in the infrastructure layer.
- **Use Cases over CRUD**: Application services orchestrate domain operations; the public surface area is expressed as use cases or commands/queries, never raw repositories or tables.
- **Test from the Inside Out**: Behavioural tests target domain and application layers using in-memory adapters. Infrastructure tests verify adapters separately with contract and integration suites.

## Layer Responsibilities
| Layer | Responsibilities | Allowed Dependencies |
|-------|------------------|-----------------------|
| **Domain** (`domain/`) | Entities, value objects, aggregates, domain services, domain events, invariants. | Standard library, domain primitives, pure utility modules. |
| **Application / Use Case** (`application/`) | Commands, queries, orchestrating domain behaviour, transactional boundaries, port definitions. | Domain layer + port interfaces + cross-cutting policies (validation, logging abstractions). |
| **Interface / Delivery** (`interface/`) | HTTP controllers, CLI handlers, gRPC endpoints, message consumers producing application commands. | Application layer + adapter factories + serialization. |
| **Infrastructure** (`infrastructure/`) | Port implementations (repositories, external services), configuration, persistence mappings, framework glue code. | Application + interface abstractions + frameworks/drivers. |
| **Configuration / Composition Root** (`main/`, `bootstrap/`) | Wire the graph of adapters and use cases, initialise framework runtimes. | All layers (for assembly only). |

Cross-cutting concerns (observability, validation policies, resilience) expose interfaces in the application layer and are implemented in infrastructure adapters.

## Suggested Directory Blueprint
```
src/
  domain/
    model/              # Entities, value objects, aggregates
    services/           # Domain services without side effects
    policies/           # Invariant and specification objects
    events/             # Domain events & serializers
  application/
    commands/
    queries/
    services/           # Use-case coordinators
    ports/              # Secondary-port interfaces (repositories, gateways)
    policies/           # Cross-cutting policies (validation, authorisation)
  interface/
    http/
    cli/
    messaging/
  infrastructure/
    persistence/
    external-services/
    messaging/
    configuration/
  main/                 # Bootstrap / composition root
tests/
  unit/
    domain/
    application/
  contract/
    ports/
  integration/
    adapters/
```
Adapt the exact paths to language norms while maintaining separation.

## Implementation Guidelines
1. **Modeling Strategy**
   - Start each bounded context by defining domain glossary, aggregate boundaries, and invariant checklists.
   - Value objects are immutable and validated at construction; aggregates enforce invariants via factory methods or named constructors.
   - Domain events capture meaningful state transitions and remain framework-free.
2. **Use Case Design**
   - Expose use cases via command/handler or query/handler pairs returning DTOs tailored for callers.
   - Use cases orchestrate domain objects and commit transactional work via ports; they never expose repositories directly.
   - Idempotency, retries, and compensating actions live here, not in controllers.
3. **Ports and Adapters**
   - Define ports in `application/ports`; name them by capability (e.g., `LedgerWriter`, `PaymentGateway`).
   - Infrastructure adapters implement ports in `infrastructure/` and receive dependencies via constructor injection.
   - Adapters map between domain DTOs and external schemas; mapping code never leaks into domain models.
4. **Transactions and Consistency**
   - Wrap each use case in an application-level unit-of-work port. Domain logic is transaction-agnostic.
   - For cross-context communication, publish domain events to an outbox port and deliver via adapters.
5. **Testing Strategy**
   - **Domain**: Pure unit tests verifying invariants and specifications.
   - **Application**: Scenario tests using in-memory or fake adapters for ports.
   - **Contract**: Validate adapter behaviour against port contracts (e.g., repository semantics, eventual consistency guarantees).
   - **Integration**: Exercise real adapters against staging or embedded infrastructure, guarded by CI policy.
6. **Documentation & Collaboration**
   - Maintain context maps and aggregate diagrams in `/docs/design/` with links back to domain models.
   - ADRs record significant changes to domain boundaries or port definitions.
   - Commit messages include bounded-context tags (e.g., `[Billing] Add SettlementAggregate`).

## Anti-Patterns to Avoid
- **Anemic Domain Models**: Application services that manipulate data classes without encapsulating invariants.
- **Layer Skipping**: Controllers instantiating or persisting entities directly.
- **Implicit Shared Kernels**: Reusing domain types across bounded contexts without explicit translation layers.
- **Leaky Adapters**: Infrastructure-specific types (ORM entities, HTTP clients) propagating into domain/application layers.
- **God Modules**: Monolithic `service` classes blending orchestration, domain logic, and IO.
- **Test Fragility**: Integration-heavy suites without stable contract or unit coverage.

## Readiness Checklist
- [ ] Bounded contexts and ubiquitous language documented and versioned.
- [ ] Each use case implemented via a distinct command/query handler with transactional boundaries defined.
- [ ] Ports expose only domain-friendly interfaces; adapters live exclusively in infrastructure.
- [ ] Domain entities/value objects encapsulate invariants and avoid infrastructure types.
- [ ] Test suite covers domain invariants, use-case workflows, adapter contracts, and critical integrations.
- [ ] Composition root wires adapters using dependency inversion; no service locator in domain/application.
- [ ] Context maps and ADRs are updated for every significant architectural decision.

## References & Related Policies
- Coordinate with `intermediate-artifacts-policy.md` for architecture documentation storage.
- Follow `technical-reasoning-policy.md` when communicating domain models or ADRs.
- Apply `verified-truth-directive.md` to maintain factual accuracy in domain documentation.
