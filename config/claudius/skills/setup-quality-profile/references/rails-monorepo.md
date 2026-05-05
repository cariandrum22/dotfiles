# Rails Monorepo Quality Profile

Use this profile for a repository that has a Rails API plus one or more additional app surfaces such
as a web frontend, browser add-on, contracts, or shared packages under a single root.

## Commit Scope Taxonomy

Recommended `scope-enum` values:

- `addon`
- `api`
- `auth`
- `ci`
- `compose`
- `config`
- `contracts`
- `deps`
- `docker`
- `docs`
- `nix`
- `packages`
- `scripts`
- `share-links`
- `tooling`
- `web`

Keep only the scopes that match real repository domains. Do NOT keep a scope for a path or concern
that does not exist.

## Workflow Naming

Use GitHub Actions workflow display names as a stable public interface.

Preferred names:

- `CI - Lint` for the repo-wide static-analysis and policy workflow
- `CI - Rails` for backend-only Rails checks when split from the repo-wide lint
- `CI - Web` or `CI - Add-on` for app-specific workflows when they exist
- `Deploy - API`, `Deploy - Web`, `Deploy - Add-on` for delivery workflows
- `Release - <Target>` only for versioning or publication flows

Avoid bare names such as `CI`, `Checks`, `Build`, or `Deploy`.

## Job Naming

Prefer explicit human-readable job `name:` fields for all checks that may be required by branch
protection.

Preferred display names:

- `Lint`
- `Ruby Scan`
- `Web Lint`
- `Add-on Lint`
- `Contract Checks`

Keep job IDs machine-oriented if desired, but keep the display name stable.

## Step Naming

Use `Verb + target` as the default pattern. Add a path qualifier in parentheses when the repository
has multiple apps.

Good examples:

- `Install Ruby gems (apps/api)`
- `Install web dependencies (apps/web)`
- `Audit web dependencies (apps/web)`
- `Build API image for Dockle`
- `Contract checks (pull_request)`
- `Lint PR title`

Avoid generic steps such as `Setup`, `Run script`, or `Checks`.

## Local Hook Naming

Prefer stable root hook names by domain:

- `ruby-lint`
- `web-lint`
- `addon-lint`
- `shell-lint`
- `docker-lint`
- `commitlint`
- `commitlint-pre-push`

Use domain names rather than tool names when the hook validates a whole app surface. This keeps the
hook stable if the command behind it changes.

## Linter Policy

### Backend

The backend baseline should include:

- RuboCop with Rails, performance, rake, minitest, and thread-safety plugins
- Sorbet when the repo uses static typing
- Brakeman
- bundler-audit
- Zeitwerk validation when it is a Rails application

### Frontend and Packages

Do NOT default to Biome for every JS or TS surface.

Prefer typed ESLint when the repo needs any of the following:

- Vue support
- GraphQL linting
- browser-extension surfaces
- security plugins
- functional-programming rules that Biome cannot express precisely

Use `/setup-typed-eslint-monorepo` for those richer surfaces.

Use Biome selectively for plain JS or TS packages that do not need those specialized checks.

## CI Policy

- Use `fetch-depth: 0` in lint workflows.
- Install dependencies before running `pre-commit`.
- Keep commitlint and PR-title lint in the repo-wide `CI - Lint` workflow.
- Add repo-specific checks such as contract validation, Docker image scanning, or GraphQL validation
  when the repository actually uses those surfaces.

## Required Checks

Keep required check names stable.

Typical minimum:

- `CI - Lint / Lint`

Add stack-specific required checks only when they are durable and clearly owned.
