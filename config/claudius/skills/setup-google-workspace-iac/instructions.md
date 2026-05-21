# Setup Google Workspace IaC

Define a safe Google Workspace and Cloud Identity infrastructure-as-code model for
OpenTofu/Terraform repositories.

Use this skill when the user wants to manage Google Workspace domains, Cloud
Identity, groups, organizational units, admin roles, SAML/OIDC applications, or
workspace security settings as code. For Google Cloud platform resources, use the
GCP section of `/setup-multicloud-iac-structure` instead.

## Existing Configuration Policy

Before changing an existing repository file, inspect the current content and ask
the user to confirm the proposed change. This applies even when the change is
additive, such as merging config keys, appending CI steps, adding package
scripts, normalizing workflow names, or updating tool versions.

Do not ask when creating a missing file from this skill's template or when the
user explicitly requested applying all changes without confirmation. Preserve
project-specific settings and avoid replacing entire files unless the user
approves that replacement.

When an existing file is involved, present a concise change plan before editing:

- `file`: target path
- `current_state`: what exists and whether this skill owns it
- `operation`: `skip`, `merge`, `update`, `replace`, or `create-adjacent`
- `proposed_delta`: exact setting, block, command, or path change to add or modify
- `risk`: compatibility, policy, or behavior risk
- `question`: the approval needed from the user

Default to `skip` or `merge`. Use `replace` only when the user explicitly
approves replacing that file.

If the user explicitly requested applying all changes without confirmation, do
not wait for approval after presenting the plan. Still follow the plan, preserve
project-specific settings, and do not use `replace` unless the user explicitly
allowed replacement.

Otherwise, if multiple existing files are affected, batch them in one plan and
wait for approval before editing any of them.

## Core Principles

- Treat the Google Workspace customer, primary domain, and Cloud Identity tenant
  as the top-level ownership boundary.
- Separate identity governance from Google Cloud project infrastructure.
- Keep users, groups, applications, and security policy in separate state
  boundaries unless the repository documents a deliberate reason to combine them.
- Prefer declarative catalogs for intended ownership and membership, but do not
  silently switch existing directories to authoritative management.
- Never manage passwords, recovery channels, backup codes, or one-time secrets in
  Terraform state.
- Keep super-admin and break-glass access small, explicit, and manually
  reviewable.
- Make OAuth scopes and domain-wide delegation grants minimal and auditable.

## Target Layout

Use this shape for new repositories or as a migration target:

```text
stacks/
  google-workspace/
    customers/<customer-id-or-domain>/
      customer.hcl
      global/
        bootstrap/
        domains/
        security/
      identity/
        organizational-units/
        groups/
        group-settings/
        admin-roles/
      applications/
        saml/<application>/
        oidc/<application>/
      devices/
        chrome/
        mobile/

modules/
  google-workspace/
    group/
    saml-app/
    admin-role/

catalog/
  identity/
    groups.yaml
    applications.yaml

docs/
  identity/

results/
```

Use provider-native identifiers in state and metadata:

- `customer_id`
- `primary_domain`
- `domain`
- `org_unit_path`
- `group_email`
- `application_id`

## State Boundary Guidance

Use separate root modules for:

- bootstrap provider access, delegated service accounts, and audit-only data
  sources
- domains and customer-wide settings
- organizational units
- groups and group settings
- group memberships when the membership set is large or imported
- admin roles and role assignments
- SAML or OIDC applications
- Chrome, mobile, or endpoint device policy

Do not combine all Google Workspace resources into one state for convenience.
Directory-wide changes can be high blast-radius, and plans become hard to
review.

## Existing Directory Safety

Before adopting an existing Google Workspace object:

1. Export or inspect the current object and its owners.
2. Decide whether the Terraform resource will be authoritative or additive.
3. Show the user the proposed import and management mode.
4. Ask the user before changing membership, admin role bindings, app assignment,
   or security policy.

For groups, explicitly classify each group:

- `authoritative`: Terraform owns the full membership set.
- `additive`: Terraform manages only selected members or settings.
- `catalog-only`: Terraform records intent but does not apply membership.

Do not convert a group to authoritative management unless the user approves the
complete membership source of truth.

## Identity Data and Artifact Policy

Treat Google Workspace exports, group membership lists, admin role assignments,
application assignments, and plan output as identity and access data even when
they are not credentials.

- Do not commit raw admin exports, user lists, group membership dumps, role
  assignment exports, application assignment exports, or unsanitized plan output
  unless the user explicitly approves committing that exact artifact.
- Commit `catalog/identity/` files only after the user confirms they are the
  intended source of truth and they have been reviewed for personal data and
  authorization exposure.
- Prefer storing raw discovery exports outside the repository, or in an
  encrypted/approved evidence location defined by the repository. If `results/`
  is used, sanitize or mask user emails, external identities, recovery metadata,
  and admin-role bindings before committing.
- Include `data_sensitivity` and `commit_allowed` in migration tables for any
  artifact derived from a live directory.

## Credential and Secret Policy

Keep credentials outside the repository:

- Use environment variables, CI secrets, or a secret manager for provider
  credentials.
- Do not commit service-account keys, OAuth client secrets, delegated admin user
  emails, refresh tokens, or `.tfvars` files containing sensitive values.
- Prefer short-lived or centrally rotated credentials where the provider and CI
  platform support them.
- Keep domain-wide delegation scopes minimal and document why each scope is
  required.

Provider configuration should read credential material from the environment or
from sensitive variables supplied by CI. Do not hard-code credentials in
`provider.tf`.

## AWS and External IdP Integration

When Google Workspace or Cloud Identity federates users into AWS:

- Keep the Google Workspace SAML/OIDC application in a Google Workspace app
  state boundary.
- Keep AWS IAM Identity Center, IAM roles, account assignments, and permission
  sets in AWS infrastructure state.
- Connect the two through explicit catalog metadata such as app name, entity ID,
  ACS URL, group mapping, and owner.
- Do not let a Google Workspace state mutate AWS account assignments directly.

This keeps identity application configuration separate from cloud account
authorization.

## Migration Workflow

1. Inventory existing domains, OUs, groups, admin roles, applications, and
   high-risk security settings.
2. Produce a migration table with `object`, `current_owner`, `target_state`,
   `management_mode`, `import_command`, `data_sensitivity`, `commit_allowed`,
   and `risk`.
3. Import one object class at a time.
4. Run plans with read-only review first and confirm destructive diffs before
   apply.
5. Move membership-heavy resources last, after the source of truth is clear.

Store only sanitized reproducible command output in `results/`. Store raw
identity exports outside the repository unless the user explicitly approves a
repository location. Store agent-only working notes in `.agents/products/` only
when the repository policy allows it.

## Validation

For each root module:

1. Run `tofu fmt -check -diff`.
2. Run `tofu init -backend=false -lockfile=readonly` when provider plugins are
   locked.
3. Run `tofu validate`.
4. Run a plan only with approved credentials and the intended state backend.

Do not run an apply until the user has reviewed the import and management-mode
table for existing objects.

## Completion Summary

When finished, report:

- state boundaries created or changed
- existing objects imported or left unmanaged
- authoritative vs additive group-management choices
- identity/access artifacts committed, sanitized, or kept outside the repository
- domain-wide delegation scopes and credential assumptions
- validation commands run
- remaining high-risk manual approvals
