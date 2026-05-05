# Scope Taxonomy

Use this reference when deriving or optimizing the `scope-enum` for
`commitlint.config.cjs`.

## What Good Scopes Look Like

A good commit scope is:

- stable across time
- small enough to fit naturally in a commit or PR title
- aligned with a real ownership or review boundary
- lower-case and preferably hyphenated
- about a domain or surface, not a one-off implementation detail

Good examples:

- `api`
- `web`
- `server`
- `client`
- `ci`
- `docs`
- `deps`
- `infra`
- `security`
- `verification`

Weak examples:

- `src`
- `misc`
- `temp`
- `cleanup`
- `bugfix`
- `feature-x`
- a single ephemeral file name

## Inference Order

When no explicit scope list is provided, infer scopes in this order:

1. existing `commitlint.config.cjs`
2. stable top-level repository structure
3. workspace boundaries (`apps/*`, `packages/*`, `crates/*`, `modules/*`,
   `envs/*`)
4. persistent cross-cutting domains (`docs`, `ci`, `scripts`, `security`,
   `release`, `tests`, `infra`, `verification`)
5. current workflow structure and release boundaries

Earlier sources should usually win unless they are clearly stale.

## Optimization Rules

### Preserve continuity

If the repo already has a useful scope list, preserve it unless:

- a scope no longer maps to any real repository area
- a scope is so broad that it communicates nothing
- several scopes should be merged into a clearer stable domain

Avoid churn just because a different name could also work.

### Keep shared concerns canonical

When these areas exist, prefer these names:

- `ci`
- `deps`
- `docs`
- `docker`
- `nix`
- `release`
- `scripts`
- `security`
- `tooling`
- `config`
- `tests`
- `infra`

### Add domain scopes only for durable surfaces

Examples:

- app monorepo: `api`, `web`, `addon`, `contracts`
- Rust platform: `server`, `client`, `ffi`, `jose`, `verification`
- infra repo: `envs`, `modules`

Do not create one scope per tiny directory unless those directories are truly
independent collaboration surfaces.

### Merge over-granular taxonomies

If the candidate list becomes too noisy:

- merge sibling implementation areas into a broader product surface
- keep a package or crate name only when it is independently owned, released, or
  reviewed
- collapse low-signal technical buckets into `tooling`, `scripts`, `config`, or
  `infra` where appropriate

### Prefer omission to noise

Because `scope-empty` is optional, a repository does not need a scope for every
possible change. If a candidate scope would be used rarely and communicates
little, leave it out.

## Repository Patterns

### App or product monorepo

Usually keep a mix of:

- shared concerns: `ci`, `deps`, `docs`, `nix`, `scripts`, `tooling`
- user-facing or service-facing surfaces: `api`, `web`, `server`, `client`,
  `addon`
- protocol or contract surfaces when they are first-class: `contracts`,
  `verification`

### Infrastructure repo

Usually keep:

- `ci`, `deps`, `docs`, `envs`, `modules`, `nix`, `scripts`, `security`,
  `tooling`

### Rust or verification-heavy monorepo

Usually keep:

- shared concerns: `ci`, `deps`, `docs`, `nix`, `scripts`, `security`, `tests`,
  `tooling`
- runtime or crate surfaces: `server`, `client`, `ffi`, `jose`
- verification surfaces when they are true top-level domains: `verification`,
  `proofs`, `fstar`, `tamarin`, `kani`

## Final Check

Before writing the config, ask:

- would a maintainer understand this scope without opening the diff?
- will this scope still make sense six months from now?
- does this scope map to a real owned domain?
- is the list concise enough to be learned by contributors?

If the answer to any of these is no, simplify the taxonomy.
