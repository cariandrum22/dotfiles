# Setup Multi-Cloud IaC Structure

Create or migrate a corporate infrastructure-as-code repository into a standardized multi-cloud,
multi-region layout for OpenTofu/Terraform and Terragrunt.

Use this skill when the user wants a durable directory standard, state-boundary model, or migration
path for AWS, Azure, and GCP infrastructure. This skill focuses on repository architecture, not
linting or CI. For quality gates, use `/setup-tofu-project` after the structure is in place.

## Core Principles

- `stacks/` contains execution units and state boundaries.
- `modules/` contains reusable modules and must not own backend state.
- Cloud provider ownership is explicit under `stacks/<cloud>/`.
- Region is a first-class rollout and state axis, not only a tag or local variable.
- Separate `global/` stacks from `regions/<provider-region>/` stacks.
- Paths use provider-native region names such as `ap-northeast-1`, `japaneast`, and
  `asia-northeast1`.
- Resource names may use short region codes, but path names and tags use full region names.
- Environment and region metadata must be explicit in `*.hcl` or variables. Do not infer them with
  fragile path regexes when writing new structure.
- One state should not manage resources in multiple regions unless the resource is genuinely global
  or a deliberate traffic-policy/control-plane stack.
- Application intent belongs in a deployment catalog; provider stacks remain the source of truth for
  provider-owned resources.

## Target Layout

Use this shape for new repositories and as the migration target for existing ones:

```text
stacks/
  aws/
    accounts/root/<OU>/<Account>/
      account.hcl
      global/<component>/
      regions/<aws-region>/<component>/
    modules/

  azure/
    tenants/<tenant>/
      tenant.hcl
      global/<component>/
      subscriptions/<subscription-or-landing-zone>/
        subscription.hcl
        global/<component>/
        regions/<azure-region>/<component>/
    modules/

  gcp/
    organizations/<org>/
      organization.hcl
      global/<component>/
      folders/<folder>/
      projects/<project>/
        project.hcl
        global/<component>/
        regions/<gcp-region>/<component>/
    modules/

modules/
  _naming/
  aws_name_adapter/
  azure_name_adapter/
  gcp_name_adapter/

catalog/
  applications/<application>/<environment>.yaml
  regions/<cloud>/<region>.yaml

docs/
  infrastructure-as-code/

results/
.agents/products/
```

## Stack File Standard

For each executable stack, prefer this minimum file set:

```text
terragrunt.hcl
README.md
versions.tf
providers.tf
locals.tf
variables.tf
outputs.tf
```

Add domain files only when needed:

```text
network.tf
security.tf
iam.tf
dns.tf
observability.tf
data.tf
```

Use kebab-case for `.tf` files and snake_case for Terraform identifiers. Generated files may be
committed only when the repository documents them as committed generated artifacts.

## Required Metadata Files

Create metadata files at durable ownership boundaries:

- `account.hcl` for AWS accounts.
- `tenant.hcl` for Azure tenants.
- `subscription.hcl` for Azure subscriptions or landing zones.
- `organization.hcl` for GCP organizations.
- `project.hcl` for GCP projects.
- `region.hcl` in each `regions/<region>/` directory when multiple regional stacks share the same
  region.
- `environment.hcl` only when environment is not already encoded by account, subscription, project,
  or catalog boundary.

Metadata should contain explicit values such as `cloud`, `environment`, `region`, `region_code`,
`account_name`, `subscription_name`, or `project_id`. Avoid deriving critical values solely from
directory names inside reusable logic.

## Global vs Regional Classification

Place these under `global/`:

- AWS Organizations, IAM Identity Center, account vending, SCPs, global IAM policy.
- Public Route53 hosted zones, registrar integration, DNSSEC governance.
- Azure Entra ID, management groups, tenant-wide policy.
- GCP organization policy, folder policy, IAM federation, project factory.
- Cross-region traffic policy where the policy itself is global.

Place these under `regions/<region>/`:

- VPC/VNet/VPC Network, subnets, route tables, TGW, Cloud WAN, peering, NAT, firewall.
- Compute, Kubernetes, databases, load balancers, private DNS resolvers, regional logs.
- FreeIPA replicas, RADIUS servers, VPN/remote-access edges, maintenance paths.
- Region-local observability and data-event logging.

If a stack contains both global and regional resources, split it before adding another region unless
the user explicitly accepts a temporary migration state.

## Deployment Catalog

Use `catalog/` to describe desired application placement without making it a provider state
boundary.

Recommended minimum:

```text
catalog/
  applications/<app>/<environment>.yaml
  regions/aws/<aws-region>.yaml
  regions/azure/<azure-region>.yaml
  regions/gcp/<gcp-region>.yaml
```

Application catalog files should describe:

- target clouds and regions
- environment
- owning team or service
- dependency order
- provider stack paths
- data residency constraints
- active/passive or active/active policy

Provider stacks may consume catalog data only through explicit variables or generated configuration
committed by the user. Do not create hidden coupling.

## Migration Workflow

1. Inspect the current repository before editing.
   - List existing stack roots and backend keys.
   - Identify cloud, account/subscription/project, environment, region, and component.
   - Identify stacks that mix global and regional resources.

2. Produce a migration map.
   - Use a table with `current_path`, `target_path`, `state_key`, `classification`, and
     `migration_risk`.
   - Store command outputs in `results/`.
   - Store only agent-only intermediate reasoning in `.agents/products/` if the local repository
     policy allows it.

3. Create the target skeleton first.
   - Add metadata files and README files.
   - Do not move real state in the same step as the first skeleton creation unless the user
     explicitly requested a one-shot migration.

4. Move one state boundary at a time.
   - Back up remote state or confirm backend versioning before changing backend keys.
   - Prefer keeping backend keys stable during directory-only moves.
   - When backend keys must change, use the tool's supported state migration flow and verify state
     resource addresses before and after.

5. Update dependencies.
   - Replace hard-coded paths with explicit dependency outputs.
   - Avoid regional stacks directly mutating global traffic or DNS policy unless they are the
     designated global policy stack.

6. Validate each wave.
   - Run format checks.
   - Run `tofu init -backend=false` and `tofu validate` where safe.
   - Run plan with the intended backend only after credentials and state ownership are confirmed.
   - The expected plan for pure directory moves is no infrastructure change.

## New Repository Workflow

For a new repository:

1. Create `stacks/aws`, `stacks/azure`, `stacks/gcp`, `modules`, `catalog`, `docs`, and `results`.
2. Add provider-specific `modules/` directories only when provider-specific modules exist.
3. Add `_naming` and provider adapter modules if the repository needs shared naming.
4. Add at least one region catalog file per governed region.
5. Add a short `docs/infrastructure-as-code/directory-structure.md` describing the state boundary
   and global/regional rules.
6. Apply `/setup-tofu-project` or component quality skills after the structure exists.

## Safety Rules

- Do not collapse all regions for one application into one state.
- Do not use `global/` for resources that are provider-regional only.
- Do not create empty placeholder project directories unless the repository has a documented
  inventory reason.
- Do not move or rename state files without an explicit migration table.
- Do not place dumps, backups, or one-off exports at repository root. Use `results/` for
  reproducible command output and a documented archive location for retained historical data.
- Do not delete old paths until the new stack has been planned and dependencies have been updated.

## Completion Summary

When finished, report:

- directories created or moved
- state boundaries changed
- global/regional splits introduced
- catalog files added
- validation commands run
- remaining migration risks
