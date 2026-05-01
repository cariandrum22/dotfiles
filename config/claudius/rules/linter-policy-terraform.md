## Linter and Formatter Configuration Policy - Terraform

### Required Tools

1. **terraform fmt** (REQUIRED - Formatter)
   - Official Terraform formatter
   - Run: `terraform fmt -recursive .`
   - Check: `terraform fmt -check -recursive .`
   - Built into Terraform CLI

2. **tflint** (REQUIRED - Linter)
   - Extensible Terraform linter
   - Run: `tflint --recursive`
   - Install: `brew install tflint` or download from GitHub

3. **tfsec** (REQUIRED - Security Scanner)
   - Static analysis security scanner
   - Run: `tfsec .`
   - Install: `brew install tfsec` or `go install github.com/aquasecurity/tfsec/cmd/tfsec@latest`

4. **checkov** (REQUIRED - Policy Scanner)
   - Comprehensive security and compliance scanner
   - Run: `checkov -d .`
   - Install: `pip install checkov`

5. **terraform-docs** (REQUIRED - Documentation)
   - Generates documentation from modules
   - Run: `terraform-docs markdown . > README.md`
   - Install: `brew install terraform-docs`

### TFLint Configuration

Create `.tflint.hcl`:
```hcl
config {
  format = "compact"
  plugin_dir = "~/.tflint.d/plugins"

  # Force explicit variable declarations
  force = false
  disabled_by_default = false
}

# Required plugins
plugin "terraform" {
  enabled = true
  preset  = "all"
}

plugin "aws" {
  enabled = true
  version = "0.27.0"
  source  = "github.com/terraform-linters/tflint-ruleset-aws"
}

plugin "azurerm" {
  enabled = true
  version = "0.25.0"
  source  = "github.com/terraform-linters/tflint-ruleset-azurerm"
}

plugin "google" {
  enabled = true
  version = "0.26.0"
  source  = "github.com/terraform-linters/tflint-ruleset-google"
}

# Core rules
rule "terraform_comment_syntax" {
  enabled = true
}

rule "terraform_documented_outputs" {
  enabled = true
}

rule "terraform_documented_variables" {
  enabled = true
}

rule "terraform_module_pinned_source" {
  enabled = true
}

rule "terraform_naming_convention" {
  enabled = true
  format = "snake_case"
}

rule "terraform_required_providers" {
  enabled = true
}

rule "terraform_required_version" {
  enabled = true
}

rule "terraform_standard_module_structure" {
  enabled = true
}

rule "terraform_typed_variables" {
  enabled = true
}

rule "terraform_unused_declarations" {
  enabled = true
}

rule "terraform_workspace_remote" {
  enabled = true
}
```

### TFSec Configuration

Create `.tfsec/config.yml`:
```yaml
minimum_severity: LOW
severity_overrides:
  aws-s3-enable-bucket-encryption: HIGH
  aws-s3-enable-bucket-logging: MEDIUM
  aws-s3-enable-versioning: HIGH
  
exclude:
  - aws-s3-enable-public-access-block  # Only if explicitly needed

custom_check_dir: .tfsec/custom_checks/
```

Create custom security check `.tfsec/custom_checks/no-hardcoded-secrets.yaml`:
```yaml
checks:
  - code: CUS001
    description: Check for hardcoded secrets in variables
    impact: Hardcoded secrets pose a security risk
    resolution: Use environment variables or secret management services
    requiredTypes:
      - resource
      - data
      - variable
    severity: CRITICAL
    matchSpec:
      name: NoHardcodedSecrets
      action: contains
      predicateMatchSpec:
        - name: secret
          action: isPresent
        - name: password
          action: isPresent
        - name: api_key
          action: isPresent
        - name: access_key
          action: isPresent
        - name: private_key
          action: isPresent
```

### Checkov Configuration

Create `.checkov.yaml`:
```yaml
# Strict security configuration
branch: main
download-external-modules: true
evaluate-variables: true
framework:
  - terraform
  - terraform_plan
  - secrets

# Ensure all checks are enabled
compact: true
output: cli
quiet: false

# Fail on any finding
soft-fail: false
hard-fail-on: 
  - CKV_AWS_*
  - CKV_AZURE_*
  - CKV_GCP_*
  - CKV_SECRET_*

# Skip specific checks only with justification
skip-check:
  # Example: - CKV_AWS_18  # S3 bucket logging (only if justified)
```

### Pre-commit Configuration

Create `.pre-commit-config.yaml`:
```yaml
repos:
  - repo: https://github.com/antonbabenko/pre-commit-terraform
    rev: v1.83.5
    hooks:
      - id: terraform_fmt
      - id: terraform_docs
        args:
          - --hook-config=--path-to-file=README.md
          - --hook-config=--add-to-existing-file=true
      - id: terraform_tflint
        args:
          - --args=--config=__GIT_WORKING_DIR__/.tflint.hcl
      - id: terraform_tfsec
        args:
          - --args=--config-file=__GIT_WORKING_DIR__/.tfsec/config.yml
      - id: terraform_validate
      - id: terraform_checkov
        args:
          - --args=--config-file=__GIT_WORKING_DIR__/.checkov.yaml

  - repo: https://github.com/Yelp/detect-secrets
    rev: v1.4.0
    hooks:
      - id: detect-secrets
        args: ['--baseline', '.secrets.baseline']
```

### Module Structure Requirements

```
terraform-module/
├── .tflint.hcl
├── .tfsec/
│   └── config.yml
├── .checkov.yaml
├── .pre-commit-config.yaml
├── .gitignore
├── README.md           # Auto-generated with terraform-docs
├── main.tf            # Main resource definitions
├── variables.tf       # All variable declarations
├── outputs.tf         # All output declarations
├── versions.tf        # Provider requirements and Terraform version
├── data.tf            # Data source queries (if any)
├── locals.tf          # Local value definitions (if any)
└── examples/          # Usage examples
    └── complete/
        ├── main.tf
        ├── variables.tf
        └── outputs.tf
```

### Best Practices Enforcement

1. **Variable Standards**:
   ```hcl
   variable "instance_type" {
     description = "EC2 instance type"
     type        = string
     default     = "t3.micro"
     
     validation {
       condition     = contains(["t3.micro", "t3.small", "t3.medium"], var.instance_type)
       error_message = "Instance type must be t3.micro, t3.small, or t3.medium."
     }
   }
   
   variable "environment" {
     description = "Environment name"
     type        = string
     
     validation {
       condition     = contains(["dev", "staging", "prod"], var.environment)
       error_message = "Environment must be dev, staging, or prod."
     }
   }
   ```

2. **Resource Tagging**:
   ```hcl
   locals {
     common_tags = {
       Environment = var.environment
       ManagedBy   = "terraform"
       Project     = var.project_name
       Owner       = var.team_email
       CostCenter  = var.cost_center
     }
   }
   
   resource "aws_instance" "example" {
     # ... other configuration ...
     tags = merge(local.common_tags, {
       Name = "${var.project_name}-${var.environment}-instance"
     })
   }
   ```

3. **Sensitive Data Handling**:
   ```hcl
   # variables.tf
   variable "database_password" {
     description = "RDS master password"
     type        = string
     sensitive   = true  # Mark as sensitive
   }
   
   # Never use default values for secrets
   # Never output sensitive values
   output "database_endpoint" {
     description = "RDS endpoint"
     value       = aws_db_instance.main.endpoint
     # Do NOT output passwords or keys
   }
   ```

4. **State Management**:
   ```hcl
   # backend.tf
   terraform {
     backend "s3" {
       bucket         = "terraform-state-bucket"
       key            = "project/environment/terraform.tfstate"
       region         = "us-east-1"
       encrypt        = true  # Always encrypt state
       dynamodb_table = "terraform-state-lock"
       
       # Enable versioning on the S3 bucket
       # Enable MFA delete protection
     }
   }
   ```

### CI/CD Integration

GitHub Actions example:
```yaml
name: Terraform Security Scan

on:
  pull_request:
    paths:
      - '**.tf'
      - '**.tfvars'

jobs:
  terraform-security:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v3
        
      - name: Terraform Format Check
        run: terraform fmt -check -recursive
        
      - name: TFLint
        uses: terraform-linters/setup-tflint@v4
        with:
          tflint_version: latest
      - run: |
          tflint --init
          tflint --recursive
          
      - name: TFSec
        uses: aquasecurity/tfsec-action@v1.0.0
        with:
          soft_fail: false
          
      - name: Checkov
        uses: bridgecrewio/checkov-action@master
        with:
          directory: .
          framework: terraform
          output_format: cli
          soft_fail: false
          
      - name: Detect Secrets
        uses: trufflesecurity/trufflehog@main
        with:
          path: ./
```

### Security-Specific Rules

1. **Encryption Requirements**:
   - All S3 buckets must have encryption enabled
   - All RDS instances must have encryption at rest
   - All EBS volumes must be encrypted
   - All data in transit must use TLS 1.2+

2. **Access Control**:
   - No public S3 buckets without explicit approval
   - No security groups with 0.0.0.0/0 ingress
   - IAM policies must follow least privilege
   - MFA must be required for privileged actions

3. **Secret Management**:
   - Use AWS Secrets Manager or HashiCorp Vault
   - Never commit secrets to version control
   - Use environment variables for sensitive data
   - Rotate secrets regularly

4. **Compliance Checks**:
   - Enable AWS Config rules
   - Use AWS CloudTrail for audit logging
   - Implement resource tagging strategy
   - Enable GuardDuty for threat detection

### Editor Integration

**VSCode**:
```json
{
  "[terraform]": {
    "editor.defaultFormatter": "hashicorp.terraform",
    "editor.formatOnSave": true,
    "editor.codeActionsOnSave": {
      "source.formatAll.terraform": true
    }
  },
  "terraform.languageServer": {
    "external": true,
    "pathToBinary": "terraform-ls",
    "args": ["serve"]
  }
}
```

**Neovim**: Use terraform-ls with nvim-lspconfig

### Terraform Version Constraints

Always specify version constraints:
```hcl
terraform {
  required_version = ">= 1.5.0, < 2.0.0"
  
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}
```