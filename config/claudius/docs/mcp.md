# Claudius MCP setup

Home Manager publishes the shared MCP definitions to `~/.config/claudius/mcpServers.json`. From the
repository root, apply the dotfiles and synchronize each agent you use:

```bash
nix run --impure .#switch
claudius config sync --global --agent claude-code
claudius config sync --global --agent codex
claudius config sync --global --agent gemini
```

Credentials below live under `${XDG_CONFIG_HOME:-$HOME/.config}/claudius/credentials/mcp/`. Each
file is loaded as an environment variable by `mcp-with-credentials`. Store `op://` references to
1Password fields in these files; the loader resolves them using `op read`. Keep credentials out of
this repository. Home Manager creates the credential directories with mode `700`.

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
gemini mcp list
gemini mcp remove codex
gemini mcp remove perplexity-ask
```

Remove corresponding project/local-scope registrations too if you created them. Keep existing
credential files until the replacement is verified. The `google-calendar` and `github` names are
retained, so syncing replaces their old launch commands.

Claude Code now uses `acceptEdits`, with automatic MCP permissions limited to AWS documentation and
Brave search. Other MCP calls use normal confirmation. Codex uses `on-request`, asks for browser
operations, and asks for tools not annotated read-only on account-connected servers. Gemini keeps
its normal MCP confirmations and disables YOLO mode. Tool annotations are hints provided by the
server; use OAuth scopes and token permissions to limit actual account access. The existing
externally sandboxed local execution configuration is retained.

Both browser servers use isolated profiles. Chrome telemetry, CrUX URL reporting, and JavaScript
evaluation are disabled, and sensitive network headers are redacted. Playwright does not expose
page-provided WebMCP tools. Its arbitrary server-code tools (`browser_run_code_unsafe` and the older
`browser_run_code`) are disabled in Codex and denied in Claude Code and Gemini policy. These
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

Claude Code's native `WebSearch` is allowed. Codex explicitly requests live native search, but the
existing Cloudflare provider's standalone-search compatibility has not been verified. No unsupported
capability is forced on the gateway. Brave remains usable with that provider; for native OpenAI
search, select the first-party provider with `codex --profile openai-search`. That profile requires
OpenAI authentication and sends model requests directly to OpenAI.

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
