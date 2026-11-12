# Claudius MCP setup

Home Manager publishes the shared MCP definitions to `~/.config/claudius/mcpServers.json`. From the
repository root, apply the dotfiles and synchronize each agent you use:

```bash
nix run --impure .#switch
claudius config sync --global --agent antigravity
claudius config sync --global --agent claude-code
claudius config sync --global --agent codex
claudius-sync-opencode --global
```

Home Manager also syncs the shared skills to Antigravity CLI (`~/.gemini/config/skills`), Claude
Code, Codex, and OpenCode (`~/.config/opencode/skills`) on activation. Antigravity CLI reads MCP
servers from `~/.gemini/config/mcp_config.json`, shared with Antigravity 2.0 and the IDE.

OpenCode receives only `playwright` and `chrome-devtools`. Home Manager runs
`claudius-sync-opencode --global` on activation. This command filters the shared definitions,
renders OpenCode configuration through Claudius, and removes all other MCP registrations (including
legacy flat entries) from the OpenCode target. It keeps a timestamped backup when the target
changes and preserves existing non-MCP settings; new targets also receive the shared OpenCode
settings. Other agents continue to receive all shared servers.
Use `claudius-sync-opencode` without `--global` for a project-local `opencode.json`.
For OpenCode, use this command instead of `claudius config sync --agent opencode`, which would
register all shared servers again. Restart an already running OpenCode session after syncing.

Credentials below live under `${XDG_CONFIG_HOME:-$HOME/.config}/claudius/credentials/mcp/`. Each
file is loaded as an environment variable by `mcp-with-credentials`. Store `op://` references to
1Password fields in these files; the loader resolves them using `op read`. Keep credentials out of
this repository. Home Manager creates the credential directories with mode `700`.

## OpenCode local model

The local llama.cpp provider's endpoint, model ID, and display name are not stored in this
repository. `config/claudius/opencode.settings.json` references them with `{file:...}` from files in
`~/.config/opencode/local-model/`, which Home Manager creates with mode `700`:

```bash
printf '%s\n' 'http://HOST:PORT/v1' > ~/.config/opencode/local-model/base-url
printf '%s\n' 'MODEL-ID' > ~/.config/opencode/local-model/model-id
printf '%s\n' 'Display name' > ~/.config/opencode/local-model/name
```

OpenCode fails to load the configuration with a `bad file reference` error while any of these
files is missing.

## Migration and permissions

The removed entries are the obsolete `codex mcp-server`, Perplexity MCP, and the community Calendar
server. Figma, Notion, and Todoist explicitly use native Streamable HTTP transport. Claudius merges
existing agent configuration, so deleting a source entry does not remove an already synchronized
entry. Before syncing, remove `codex` and `perplexity-ask` from each agent where they appear:

```bash
claude mcp list
claude mcp remove codex --scope user
claude mcp remove perplexity-ask --scope user
codex mcp list
codex mcp remove codex
codex mcp remove perplexity-ask
```

Remove corresponding project/local-scope registrations too if you created them. Keep existing
credential files until the replacement is verified. The `google-calendar` and `github` names are
retained, so syncing replaces their old launch commands.

Claude Code now uses `acceptEdits`, with automatic MCP permissions limited to AWS documentation and
Brave search. Other MCP calls use normal confirmation. Codex uses `on-request`, asks for browser
operations, and asks for tools not annotated read-only on account-connected servers. Antigravity CLI
keeps its normal MCP confirmations. Tool annotations are hints provided by the
server; use OAuth scopes and token permissions to limit actual account access. The existing
externally sandboxed local execution configuration is retained.

Both browser servers use isolated profiles. Chrome telemetry, CrUX URL reporting, and JavaScript
evaluation are disabled, and sensitive network headers are redacted. Playwright does not expose
page-provided WebMCP tools. Its arbitrary server-code tools (`browser_run_code_unsafe` and the older
`browser_run_code`) are disabled in Codex and denied in Claude Code and Antigravity CLI
(`permissions.deny` in `antigravity.settings.json`). These
settings do not make browser automation a security boundary. Isolated profiles discard login state
when closed; use them for testing rather than a personal browser session.

## Search

General-purpose search uses
[Brave's official MCP server](https://github.com/brave/brave-search-mcp-server), not the deprecated
`@modelcontextprotocol/server-brave-search` package. Web, news, image, video, and local search are
enabled; Brave's summarizer is excluded because the agent can synthesize results. The specialized
official AWS documentation server remains available.

Create a Brave Search API key and save its 1Password reference:

```bash
config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/claudius"
umask 077
mkdir -p "$config_dir/credentials/mcp/brave-search"
printf '%s\n' 'op://Automation/Brave Search MCP/api_key' \
  > "$config_dir/credentials/mcp/brave-search/BRAVE_API_KEY"
```

Brave API billing is separate from a consumer search/browser subscription. Perplexity Pro and
Perplexity API billing are also separate: removing its MCP or cancelling Pro does not disable API
charges. Review API keys and automatic credit purchases in Perplexity's console after confirming the
replacement. This repository does not change subscriptions or billing settings.

Claude Code's native `WebSearch` is allowed. Codex explicitly requests live native search and defaults
to the first-party OpenAI provider in both the desktop app and CLI. OpenAI authentication is required;
model requests go directly to OpenAI. Select Cloudflare AI Gateway explicitly with
`codex --profile cloudflare`. The gateway's standalone-search compatibility has not been verified;
Brave remains usable with either provider.

The supported launch variants below inherit
`model = "gpt-6.1-sol"`, `review_model = "gpt-6.1-sol"`, and `model_reasoning_effort = "high"`
from the shared settings. Daybreak is off in the shared settings and enabled by the explicit profiles:

| Invocation | Provider | Daybreak |
| --- | --- | --- |
| `openai` | OpenAI, saved Codex login | Off |
| `openai --daybreak` | OpenAI, saved Codex login | On |
| `codex --profile cloudflare` | Cloudflare AI Gateway | Off |
| `codex-api` | OpenAI, locally configured API key | Off |
| `codex-api --daybreak` | OpenAI, locally configured API key | On |

`openai` is a Home Manager launcher available in Elvish and other shells. It calls the Codex
executable directly with the built-in OpenAI provider and saved authentication, without invoking the
shell's Claudius wrapper. `openai --daybreak` selects `openai-daybreak.config.toml`. Plain `codex`
still uses the same shared model defaults. All variants inherit live search from the shared settings;
separate search profiles are unnecessary.

The Daybreak profiles set `daybreak = true` and `features.api_key_cyber_access_programs = true`.
The latter allows sessions authenticated with an API key to send an explicit access-program
selection to OpenAI. Exporting `OPENAI_API_KEY` alone does not switch the built-in provider's
interactive session away from saved ChatGPT login. The API project must already have access to both
Daybreak and the selected model; these flags do not grant access. See the
[OpenAI configuration reference](https://learn.chatgpt.com/docs/config-file/config-reference) and
[Daybreak API guide](https://developers.openai.com/api/docs/guides/daybreak).

Codex 0.166.0 only forwards explicit cyber access programs for the built-in `openai` provider.
Adding the same flags to `cloudflare_openai` would silently omit that selection, so no nonfunctional
`cloudflare-daybreak` profile is installed. `openai_base_url` can change the built-in provider's
endpoint, but its provider overrides do not accept the `cf-aig-authorization` header needed by this
gateway configuration. Gateway Daybreak support requires a Codex change or an authenticated proxy;
use `openai --daybreak` for direct OpenAI requests in the meantime.

The former `codex --profile openai`, `openai-search`, and `openai-search-daybreak` profiles are
retired. Home Manager moves any remaining files to `*.config.toml.retired.<timestamp>.<pid>` on
activation, preserving personal settings while removing the old profile names. Use `openai` or
`openai --daybreak` instead; the former personal Astra/xhigh model override is no longer loaded.
Codex 0.134+ uses these separate files instead of `[profiles.*]` tables or a top-level
`profile` selector in `config.toml`. Remove the legacy `[profiles.openai-search]` table from an existing
user config when migrating, since Claudius sync may preserve settings absent from its source.
After applying Home Manager and syncing Codex, restart the desktop app and start a new chat to use the
updated default provider.

### Isolated OpenAI API sessions

Use `codex-api` or `codex-api --daybreak` for an API-authenticated session with separate
Codex authentication and history. The launcher resolves only the configured OpenAI key through
Claudius; Cloudflare credentials are not required.

Keep the 1Password reference in a local file outside this repository. Replace the example below
with your own vault, item, and field names:

```bash
config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/claudius"
umask 077
mkdir -p "$config_dir/credentials/codex-api"
printf '%s\n' 'op://Example Vault/Example API Key/credential' \
  > "$config_dir/credentials/codex-api/OPENAI_API_KEY"
codex-api
codex-api --daybreak
```

The file must contain one `op://` reference, not the API key itself. It is read at runtime and is
not included in the Nix build inputs. The existing Elvish and Fish Claudius wrappers read the same
file instead of embedding a vault item name. Restart those shells after applying Home Manager.

Both variants use the built-in `openai` provider at `https://api.openai.com/v1`, GPT-6.1 Sol with
`high` reasoning, and the shared MCP definitions and policies. Home Manager renders the complete
configuration through Claudius into `~/.codex-api/config.toml`; its `daybreak.config.toml`
profile selects Daybreak and the API-key feature flag. Installing this profile does not establish
or grant program access; availability depends on the API project's approvals.

Home Manager merges the generated API configuration and the Codex profiles into writable local
files on activation. Codex persists project trust and UI choices in the selected configuration,
so these files must not be symlinks into the read-only Nix store. Activation refreshes managed
settings while preserving local additions such as project trust. The source defaults remain
declarative in this repository.

Each writable target has a private `*.managed.json` sidecar recording the previous managed
settings. Activation removes those owned settings before applying the new defaults, so deleting
a declaration also removes it from the local configuration. MCP server and model provider
definitions are owned as complete entries; other tables retain locally added keys. On first
adoption, existing keys absent from the declarations are preserved because their ownership is
unknown. Keep the sidecar alongside the configuration to retain deletion tracking. An interrupted
sync records both generations so the next activation can recover.

Codex 0.166.0's interactive mode requires saved API-key authentication. On each launch, the helper
passes the resolved key to `codex login --with-api-key` through stdin, then starts a fresh Codex
process in that dedicated home. Codex stores the API key in its private `auth.json`; login state,
conversation history, and MCP OAuth state are separate from `~/.codex`. The normal ChatGPT login
is preserved. The helper removes the key from the launched session's environment after login.
Shared skills remain available from `~/.agents/skills`; plugins and MCP OAuth connections that
require sign-in need to be configured separately in the API home. `CODEX_API_HOME` can select
another already-configured dedicated home.

### Local Elvish commands

Organization-specific command names and dispatchers belong in the optional local module
`~/.config/elvish/lib/local.elv`, outside this repository. Home Manager does not generate this
file. The interactive shell registers the closures in its `commands` map; names include the
Elvish function-variable suffix `~`. For example:

```elvish
fn work {|@args|
  e:codex-api $@args
}
var commands = [&work~=$work~]
```

Extend that local module with additional subcommands or wrappers as needed. Keep real organization
names, account associations, and credential references out of public examples and pull requests.

## GitHub

GitHub uses the [official local MCP server](https://github.com/github/github-mcp-server), supplied
by the pinned Nixpkgs input, with `--lockdown-mode`. Its existing
`github/GITHUB_PERSONAL_ACCESS_TOKEN` credential is loaded into the server's environment. The PAT is
never expanded into process arguments, and the third-party HTTP bridge is removed. Prefer a
fine-grained PAT limited to the repositories and operations you need. When running the shared
configuration without Home Manager, install `github-mcp-server` separately.

## Runtime versions

Local npm/Python server versions are explicit; review upstream release notes and update the pins
together with launcher checks. The Google MCP service itself is official; its `mcp-remote` OAuth
bridge is third-party and is also pinned.

| Runtime                            | Version                        |
| ---------------------------------- | ------------------------------ |
| AWS documentation                  | 1.2.2                          |
| Brave Search                       | 2.1.4                          |
| Chrome DevTools                    | 1.10.1                         |
| Playwright                         | 0.0.83                         |
| Google OAuth bridge (`mcp-remote`) | 0.14.3                         |
| X OAuth bridge (`xurl`)            | 1.3.4                          |
| GitHub                             | Nixpkgs lock (currently 1.1.2) |

## Personal Google account

`google-personal` uses the third-party [Workspace MCP](https://github.com/taylorwilsdon/google_workspace_mcp)
package pinned to 2.0.1, running locally over stdio with only Gmail, Calendar, and Drive enabled.
The launcher uses `--read-only`, limiting OAuth scopes and removing write tools. The official
Developer Preview remote servers remain configured for a later Workspace setup, but are disabled
in Codex while testing the personal account.

Enable Gmail API, Google Calendar API, and Google Drive API in the personal Cloud project.
Create an External OAuth app, add the personal Gmail address as a test user, and create a Web
application client with `http://localhost:8000/oauth2callback` as its redirect URI.
Store `client_id` and `client_secret` in the `Google OAuth - MCP - Personal` 1Password item.
Put their `op://` references in `credentials/mcp/google-personal/GOOGLE_OAUTH_CLIENT_ID` and
`credentials/mcp/google-personal/GOOGLE_OAUTH_CLIENT_SECRET`. Never put the values in this repo.

The private token cache is `~/.config/claudius/credentials/google-personal`, separate from Workspace
credentials. Only authenticate the personal account in this instance. External apps in Testing
normally receive refresh tokens that expire after seven days with these scopes.

## Google Workspace

The `google-gmail`, `google-drive`, and `google-calendar` entries connect to Google's official
remote MCP servers through `mcp-remote`, sharing one OAuth client. The old
`@cocal/google-calendar-mcp` entry is replaced while keeping the `google-calendar` server name.

As of 2026-10-04, these servers require membership in the
[Google Workspace Developer Preview Program](https://developers.google.com/workspace/preview).
Complete the prerequisites in the
[official setup guide](https://developers.google.com/workspace/guides/configure-mcp-servers) before
using the migrated calendar.

1. Enable the APIs and MCP services in your Google Cloud project:

   ```bash
   gcloud services enable \
     gmail.googleapis.com drive.googleapis.com calendar-json.googleapis.com \
     gmailmcp.googleapis.com drivemcp.googleapis.com calendarmcp.googleapis.com \
     --project=YOUR_PROJECT_ID
   ```

2. Configure the OAuth consent screen and add your account as a test user if the app is external and
   in testing. Create an OAuth client of type **Web application** with these redirect URIs:
   - `http://localhost:3335/oauth/callback` (Gmail)
   - `http://localhost:3336/oauth/callback` (Drive)
   - `http://localhost:3337/oauth/callback` (Calendar)

3. Add the scopes used by `bin/mcp-google-workspace` to the consent screen:
   - Gmail: `gmail.readonly` and `gmail.compose` (read mail and manage drafts; the compose scope
     also grants permission to send mail).
   - Drive: `drive.readonly` and `drive.file` (read existing files and manage files created or
     authorized through the app).
   - Calendar: `calendar.calendarlist.readonly`, `calendar.events`, and `calendar.events.freebusy`
     (list calendars, read and manage events, and query availability).

   Each scope above has the prefix `https://www.googleapis.com/auth/`. Calendar uses
   `calendar.events` to retain event creation and updates after migration.

4. Create `google-workspace/GOOGLE_CLIENT_ID` and `google-workspace/GOOGLE_CLIENT_SECRET` with
   references to the new OAuth client, for example:

   ```bash
   config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/claudius"
   umask 077
   mkdir -p "$config_dir/credentials/mcp/google-workspace"
   printf '%s\n' 'op://Automation/Google Workspace MCP/client_id' \
     > "$config_dir/credentials/mcp/google-workspace/GOOGLE_CLIENT_ID"
   printf '%s\n' 'op://Automation/Google Workspace MCP/client_secret' \
     > "$config_dir/credentials/mcp/google-workspace/GOOGLE_CLIENT_SECRET"
   ```

5. Authenticate each service once before starting the agent, allowing its browser flow to finish:

   ```bash
   config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/claudius"
   "$config_dir/bin/mcp-with-credentials" --server google-workspace -- \
     "$config_dir/bin/mcp-google-workspace" gmail
   # Stop with Ctrl-C after authentication, then repeat with drive and calendar.
   ```

`mcp-remote` caches OAuth tokens in `~/.mcp-auth` and requests offline access for refresh. Keep that
directory private. On a remote host, forward the corresponding callback port over SSH to complete
the browser login. The three distinct ports allow the services to run together.

The previous `google-calendar/GOOGLE_OAUTH_CREDENTIALS` file and the old Calendar MCP token cache
are left intact, but the official servers do not use them. This migration requires fresh OAuth
consent; copying the previous token cache is not supported.

## X

The `x` entry uses X's official `@xdevplatform/xurl` OAuth bridge to
[`https://api.x.com/mcp`](https://docs.x.com/tools/mcp).

1. Create an X developer app with OAuth 2.0 enabled and register `http://localhost:8080/callback` as
   its redirect URI. X requires the app to use the **Pay-per-use** package and **Production**
   environment.
2. Create `x/CLIENT_ID` and `x/CLIENT_SECRET` under the credential directory, using `op://`
   references to the app's credentials.
3. Authenticate before starting the agent:

   ```bash
   config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/claudius"
   "$config_dir/bin/mcp-with-credentials" --server x -- \
     npx -y @xdevplatform/xurl@1.3.4 auth oauth2
   ```

   On a headless host, append `--headless` and complete the login using the displayed URL.

`xurl` caches and refreshes tokens in `~/.xurl`. MCP access includes post search, user lookup,
bookmarks, trends, and Articles; see the official guide for the current tool list.

## Multiple accounts

A single Codex can use multiple named MCP instances, such as `x-personal`, `x-work`,
`google-gmail-personal`, and `google-gmail-work`. The definitions here configure one X account and
one Google OAuth client; no additional accounts are authenticated by applying the dotfiles.

For Google, duplicate the relevant entries under distinct names and change
`--server google-workspace` to a separate credential group such as `google-workspace-work`. In that
group's credential files, set `MCP_REMOTE_CONFIG_DIR` to an absolute private directory for that
account's token cache. Set `GOOGLE_GMAIL_CALLBACK_PORT`, `GOOGLE_DRIVE_CALLBACK_PORT`, and
`GOOGLE_CALENDAR_CALLBACK_PORT` to unused ports (for example, 3435, 3436, and 3437) and register the
matching redirect URIs with the OAuth client. The launcher validates these overrides. Each account
needs its own consent flow, even when the same OAuth client is used.

For X, authenticate named users with `xurl auth oauth2 USERNAME --app APP_NAME`, then add
`--app APP_NAME --username USERNAME` to each instance's `xurl mcp` command. `xurl` selects the
cached user token explicitly. App selection alone does not distinguish users sharing that app.

Add the new names to agent allowlists and copy the account-server approval policies when adding
instances. Include the intended account in requests that write mail, events, files, or posts.
