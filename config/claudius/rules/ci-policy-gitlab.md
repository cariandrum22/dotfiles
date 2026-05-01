## CI Policy - GitLab CI/CD

> This document extends the requirements defined in `ci-policy-common.md`. Review that baseline first, then apply the GitLab-specific guidance below.

### Local Testing Requirements

1. **gitlab-runner** (REQUIRED - Local GitLab CI Runner)
   - Test pipelines locally before pushing
   - Install: `brew install gitlab-runner` or `curl -L https://packages.gitlab.com/install/repositories/runner/gitlab-runner/script.deb.sh | sudo bash`
   - Documentation: https://docs.gitlab.com/runner/

### GitLab Runner Configuration

Register local runner:
```bash
# Register runner for local execution
gitlab-runner register \
  --non-interactive \
  --url "https://gitlab.com/" \
  --registration-token "PROJECT_REGISTRATION_TOKEN" \
  --executor "docker" \
  --docker-image "alpine:latest" \
  --description "local-runner" \
  --tag-list "local" \
  --run-untagged="true" \
  --locked="false"
```

Create `.gitlab-runner-config.toml`:
```toml
concurrent = 1
check_interval = 0

[session_server]
  session_timeout = 1800

[[runners]]
  name = "local-docker"
  url = "https://gitlab.com/"
  token = "YOUR_RUNNER_TOKEN"
  executor = "docker"
  [runners.custom_build_dir]
  [runners.cache]
    [runners.cache.s3]
    [runners.cache.gcs]
    [runners.cache.azure]
  [runners.docker]
    tls_verify = false
    image = "alpine:latest"
    privileged = false
    disable_entrypoint_overwrite = false
    oom_kill_disable = false
    disable_cache = false
    volumes = ["/cache", "/var/run/docker.sock:/var/run/docker.sock"]
    shm_size = 0
```

### Pipeline Structure Best Practices

Create `.gitlab-ci.yml`:
```yaml
# Pipeline configuration
workflow:
  rules:
    - if: $CI_PIPELINE_SOURCE == "merge_request_event"
    - if: $CI_COMMIT_BRANCH && $CI_OPEN_MERGE_REQUESTS
      when: never
    - if: $CI_COMMIT_BRANCH

# Global variables
variables:
  FF_USE_FASTZIP: "true"
  ARTIFACT_COMPRESSION_LEVEL: "fast"
  CACHE_COMPRESSION_LEVEL: "fast"
  DOCKER_DRIVER: overlay2
  DOCKER_TLS_CERTDIR: ""
  NODE_VERSION: "20"
  PYTHON_VERSION: "3.11"

# Global cache configuration
cache:
  key:
    files:
      - package-lock.json
      - yarn.lock
      - pnpm-lock.yaml
  paths:
    - node_modules/
    - .npm/
    - .yarn/
    - .pnpm-store/

# Stages definition
stages:
  - prepare
  - validate
  - test
  - build
  - security
  - deploy
  - cleanup

# Templates for reuse
.node_template: &node_template
  image: node:${NODE_VERSION}-alpine
  before_script:
    - npm config set registry https://registry.npmjs.org/
    - npm ci --cache .npm --prefer-offline
  cache:
    policy: pull

# Prepare stage
install:dependencies:
  stage: prepare
  <<: *node_template
  cache:
    policy: pull-push
  script:
    - npm ci
  artifacts:
    expire_in: 1 hour
    paths:
      - node_modules/

# Validation stage - parallel jobs
lint:code:
  stage: validate
  <<: *node_template
  needs: ["install:dependencies"]
  script:
    - npm run lint
  rules:
    - if: $CI_MERGE_REQUEST_ID
    - if: $CI_COMMIT_BRANCH == $CI_DEFAULT_BRANCH

lint:styles:
  stage: validate
  <<: *node_template
  needs: ["install:dependencies"]
  script:
    - npm run lint:styles
  parallel:
    matrix:
      - LINTER: [stylelint, prettier]

# Security scanning
secret:detection:
  stage: security
  image: 
    name: trufflesecurity/trufflehog:latest
    entrypoint: [""]
  script:
    - trufflehog filesystem . --json --fail
  allow_failure: false
  rules:
    - if: $CI_MERGE_REQUEST_ID
    - if: $CI_COMMIT_BRANCH == $CI_DEFAULT_BRANCH

dependency:scanning:
  stage: security
  image: 
    name: aquasec/trivy:latest
    entrypoint: [""]
  script:
    - trivy fs --security-checks vuln,config --exit-code 1 --severity HIGH,CRITICAL .
  artifacts:
    reports:
      dependency_scanning: gl-dependency-scanning-report.json
  dependencies:
    - install:dependencies

sast:
  stage: security
  include:
    - template: Security/SAST.gitlab-ci.yml

# Test stage with coverage
test:unit:
  stage: test
  <<: *node_template
  needs: ["install:dependencies"]
  coverage: '/Lines\s+:\s+(\d+\.\d+)%/'
  script:
    - npm run test:ci
  artifacts:
    when: always
    reports:
      junit:
        - junit.xml
      coverage_report:
        coverage_format: cobertura
        path: coverage/cobertura-coverage.xml
    paths:
      - coverage/
    expire_in: 1 week

test:integration:
  stage: test
  image: docker:24-dind
  services:
    - docker:24-dind
  variables:
    DOCKER_HOST: tcp://docker:2376
  script:
    - docker compose -f docker-compose.test.yml up --abort-on-container-exit
  needs: ["build:docker"]

# Build stage
build:application:
  stage: build
  <<: *node_template
  needs: ["test:unit"]
  script:
    - npm run build
  artifacts:
    paths:
      - dist/
    expire_in: 1 week

build:docker:
  stage: build
  image: docker:24-dind
  services:
    - docker:24-dind
  needs: ["build:application"]
  before_script:
    - docker login -u $CI_REGISTRY_USER -p $CI_REGISTRY_PASSWORD $CI_REGISTRY
  script:
    - |
      docker buildx create --use
      docker buildx build \
        --cache-from $CI_REGISTRY_IMAGE:cache \
        --cache-to $CI_REGISTRY_IMAGE:cache \
        --tag $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA \
        --tag $CI_REGISTRY_IMAGE:latest \
        --push .
  rules:
    - if: $CI_COMMIT_BRANCH == $CI_DEFAULT_BRANCH

# Deployment stages
.deploy_template: &deploy_template
  image: alpine:latest
  before_script:
    - apk add --no-cache curl openssh-client
    - eval $(ssh-agent -s)
    - echo "$SSH_PRIVATE_KEY" | ssh-add -
    - mkdir -p ~/.ssh
    - ssh-keyscan -H $DEPLOY_HOST >> ~/.ssh/known_hosts

deploy:staging:
  <<: *deploy_template
  stage: deploy
  environment:
    name: staging
    url: https://staging.example.com
    on_stop: stop:staging
  script:
    - |
      ssh $DEPLOY_USER@$DEPLOY_HOST << EOF
        docker pull $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA
        docker stop app-staging || true
        docker run -d --name app-staging \
          -p 3000:3000 \
          --env-file /etc/app/staging.env \
          $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA
      EOF
  only:
    - develop
  when: manual

deploy:production:
  <<: *deploy_template
  stage: deploy
  environment:
    name: production
    url: https://app.example.com
  script:
    - |
      ssh $DEPLOY_USER@$DEPLOY_HOST << EOF
        docker pull $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA
        docker stop app-blue || true
        docker rename app-green app-blue || true
        docker run -d --name app-green \
          -p 3001:3000 \
          --env-file /etc/app/production.env \
          $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA
        # Health check
        sleep 10
        curl -f http://localhost:3001/health || exit 1
        # Switch traffic
        docker stop app-blue
        docker run -d --name app-production \
          -p 3000:3000 \
          --env-file /etc/app/production.env \
          $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA
      EOF
  only:
    - main
  when: manual
  needs: ["deploy:staging"]

# Include external templates
include:
  - local: '.gitlab/ci/security.yml'
  - local: '.gitlab/ci/quality.yml'
  - template: Code-Quality.gitlab-ci.yml
```

### Local Testing Commands

```bash
# Validate CI configuration
gitlab-runner exec shell validate

# List available jobs
gitlab-runner exec docker --list

# Run specific job locally
gitlab-runner exec docker test:unit

# Run with environment variables
gitlab-runner exec docker test:unit \
  --env "CI_COMMIT_SHA=abc123" \
  --env "CI_COMMIT_BRANCH=main"

# Run with custom Docker image
gitlab-runner exec docker test:unit \
  --docker-image node:20-alpine

# Run with artifacts from previous job
gitlab-runner exec docker build:application \
  --docker-volumes /tmp/artifacts:/artifacts

# Debug mode
gitlab-runner --debug exec docker test:unit

# Run entire pipeline locally (sequentially)
for job in $(gitlab-runner exec docker --list | grep -v "STAGE"); do
  gitlab-runner exec docker $job || break
done
```

### Advanced Configuration

Create `.gitlab/ci/security.yml`:
```yaml
# Security-specific jobs
container:scanning:
  stage: security
  image: 
    name: aquasec/trivy:latest
    entrypoint: [""]
  services:
    - docker:24-dind
  variables:
    DOCKER_HOST: tcp://docker:2375
    DOCKER_DRIVER: overlay2
    DOCKER_TLS_CERTDIR: ""
  script:
    - trivy image --exit-code 1 --severity HIGH,CRITICAL $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA
  artifacts:
    reports:
      container_scanning: gl-container-scanning-report.json
  rules:
    - if: $CI_COMMIT_BRANCH == $CI_DEFAULT_BRANCH

license:scanning:
  stage: security
  image: 
    name: licensefinder/license_finder:latest
    entrypoint: [""]
  script:
    - license_finder
  artifacts:
    reports:
      license_scanning: gl-license-scanning-report.json
  allow_failure: true
```

### Performance Optimization

1. **DAG (Directed Acyclic Graph)**:
   ```yaml
   test:unit:
     needs: []  # Start immediately
     
   test:integration:
     needs: ["build:docker"]  # Only after Docker build
     
   deploy:staging:
     needs: ["test:unit", "test:integration", "security:scan"]
   ```

2. **Parallel Matrix Jobs**:
   ```yaml
   test:browsers:
     parallel:
       matrix:
         - BROWSER: [chrome, firefox, safari]
           OS: [ubuntu, macos]
     script:
       - npm run test:e2e -- --browser=$BROWSER
   ```

3. **Dynamic Child Pipelines**:
   ```yaml
   generate:tests:
     stage: prepare
     script:
       - |
         cat > child-pipeline.yml << EOF
         test:dynamic:
           script: echo "Dynamic test"
         EOF
     artifacts:
       paths:
         - child-pipeline.yml
         
   trigger:tests:
     stage: test
     needs: ["generate:tests"]
     trigger:
       include:
         - artifact: child-pipeline.yml
           job: generate:tests
       strategy: depend
   ```

### GitLab-Specific Features

1. **Review Apps**:
   ```yaml
   review:app:
     stage: deploy
     environment:
       name: review/$CI_COMMIT_REF_SLUG
       url: https://$CI_COMMIT_REF_SLUG.review.example.com
       on_stop: stop:review
       auto_stop_in: 1 week
     script:
       - deploy_review_app.sh
     only:
       - merge_requests
   ```

2. **Multi-Project Pipelines**:
   ```yaml
   trigger:downstream:
     stage: deploy
     trigger:
       project: group/downstream-project
       branch: main
       strategy: depend
     variables:
       UPSTREAM_SHA: $CI_COMMIT_SHA
   ```

3. **Scheduled Pipelines**:
   ```yaml
   nightly:security:scan:
     script:
       - npm audit
       - trivy fs .
     only:
       - schedules
     variables:
       SCAN_TYPE: "comprehensive"
   ```

### Troubleshooting

```bash
# Debug job locally
gitlab-runner --debug exec docker job_name

# Check runner configuration
gitlab-runner verify

# Clear runner cache
gitlab-runner cache-clear

# Run with specific runner config
gitlab-runner exec docker job_name \
  --config /path/to/config.toml

# Interactive shell for debugging
gitlab-runner exec shell job_name \
  --docker-volumes $PWD:/builds/project

# Test with GitLab CI Lint
curl --header "Content-Type: application/json" \
     --header "PRIVATE-TOKEN: $GITLAB_TOKEN" \
     --data '{"content": "'"$(cat .gitlab-ci.yml | jq -sR .)"'"}' \
     "https://gitlab.com/api/v4/ci/lint"
```

### Security Best Practices

1. **Protected Variables**:
   ```yaml
   deploy:production:
     script:
       - echo $PROD_API_KEY  # Only available in protected branches
     only:
       - main
     variables:
       GIT_STRATEGY: none  # Don't clone for deployment
   ```

2. **OIDC Token**:
   ```yaml
   aws:deploy:
     id_tokens:
       AWS_TOKEN:
         aud: https://gitlab.com
     script:
       - aws sts assume-role-with-web-identity \
           --role-arn $AWS_ROLE_ARN \
           --role-session-name gitlab-ci \
           --web-identity-token $AWS_TOKEN
   ```

3. **Artifact Security**:
   ```yaml
   secure:artifacts:
     script:
       - generate_reports.sh
     artifacts:
       paths:
         - reports/
       expire_in: 1 day
       reports:
         dotenv: build.env  # Masked in logs
   ```

### Required Files

Create `.gitlab/ci/templates.yml`:
```yaml
# Reusable job templates
.cache:node:
  cache:
    key:
      files:
        - package-lock.json
    paths:
      - node_modules/
      - .npm/
    policy: pull

.only:changes:
  rules:
    - changes:
        - "**/*.{js,jsx,ts,tsx}"
        - package*.json
        - .gitlab-ci.yml
```
