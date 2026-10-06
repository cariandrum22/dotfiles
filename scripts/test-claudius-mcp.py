"""Exercise credential launchers and agent rendering without network or real tokens."""

from __future__ import annotations

import json
import os
import shlex
import shutil
import subprocess  # noqa: S404 - Executes only local fixtures and Claudius.
import sys
import tempfile
import tomllib
import unittest
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[1] / "config/claudius"
UNSAFE_TOOLS = {"browser_run_code", "browser_run_code_unsafe"}
GOOGLE_SERVICES = ("gmail", "drive", "calendar")


class McpRegressionTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory(prefix="claudius mcp ", dir=Path.home())
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.config = self.root / "config/claudius"
        self.config.mkdir(parents=True)
        for path in SOURCE.glob("*.json"):
            shutil.copy2(path, self.config / path.name)
        for path in SOURCE.glob("*.toml"):
            shutil.copy2(path, self.config / path.name)
        shutil.copytree(SOURCE / "bin", self.config / "bin")
        # Nix build sandboxes have no /usr/bin/env; use the available Bash for
        # fixture shebangs while exercising the original launcher arguments.
        bash = shutil.which("bash")
        self.assertIsNotNone(bash)
        for launcher in (self.config / "bin").glob("mcp-*"):
            launcher.chmod(0o700)
            content = launcher.read_text().split("\n", 1)[1]
            launcher.write_text(f"#!{bash}\n{content}")
        self.env = os.environ | {"XDG_CONFIG_HOME": str(self.root / "config")}
        for name in (
            "GOOGLE_CLIENT_ID",
            "GOOGLE_CLIENT_SECRET",
            "GITHUB_PERSONAL_ACCESS_TOKEN",
            "BRAVE_API_KEY",
            "CLIENT_ID",
            "CLIENT_SECRET",
            "MCP_REMOTE_CONFIG_DIR",
            "GOOGLE_GMAIL_CALLBACK_PORT",
            "GOOGLE_DRIVE_CALLBACK_PORT",
            "GOOGLE_CALENDAR_CALLBACK_PORT",
        ):
            self.env.pop(name, None)
        self.servers = json.loads((self.config / "mcpServers.json").read_text())[
            "mcpServers"
        ]

    def run_command(self, command: list[str]) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            command,
            cwd=self.root,
            env=self.env,
            capture_output=True,
            text=True,
            timeout=30,
            check=True,
        )

    def install_mocks(self) -> Path:
        mock_bin = self.root / "mock bin"
        mock_bin.mkdir()
        capture = self.root / "capture.json"
        stub = (
            f"#!{sys.executable}\n"
            "import json, os, sys\n"
            "from pathlib import Path\n"
            "if Path(sys.argv[0]).name == 'op':\n"
            "    print('fixture-resolved-token')\n"
            "else:\n"
            "    values = {k: os.environ.get(k) for k in "
            "['GITHUB_PERSONAL_ACCESS_TOKEN', 'GOOGLE_CLIENT_ID', "
            "'GOOGLE_CLIENT_SECRET', 'BRAVE_API_KEY', 'CLIENT_SECRET']}\n"
            "    Path(os.environ['MCP_TEST_CAPTURE']).write_text("
            "json.dumps({'argv': sys.argv[1:], 'env': values}))\n"
        )
        for name in ("npx", "github-mcp-server", "op"):
            path = mock_bin / name
            path.write_text(stub)
            path.chmod(0o755)
        bash_env = self.root / "bash env"
        bash_env.write_text(f'export PATH={shlex.quote(str(mock_bin))}:"$PATH"\n')
        self.env |= {"BASH_ENV": str(bash_env), "MCP_TEST_CAPTURE": str(capture)}
        return capture

    def credential(self, group: str, name: str, value: str) -> None:
        directory = self.config / "credentials/mcp" / group
        directory.mkdir(parents=True, exist_ok=True)
        (directory / name).write_text(value + "\n")

    def launch(self, name: str) -> None:
        server = self.servers[name]
        self.run_command([server["command"], *server["args"]])

    def test_github_token_is_resolved_without_entering_argv(self) -> None:
        capture = self.install_mocks()
        self.credential("github", "GITHUB_PERSONAL_ACCESS_TOKEN", "op://test/token")
        self.launch("github")
        result = json.loads(capture.read_text())
        self.assertEqual(
            result["env"]["GITHUB_PERSONAL_ACCESS_TOKEN"],
            "fixture-resolved-token",
        )
        self.assertNotIn("fixture-resolved-token", json.dumps(result["argv"]))
        self.assertEqual(result["argv"], ["stdio", "--lockdown-mode"])

    def test_google_launches_hide_secrets_and_use_distinct_ports(self) -> None:
        capture = self.install_mocks()
        self.credential("google-workspace", "GOOGLE_CLIENT_ID", "fixture-client-id")
        self.credential("google-workspace", "GOOGLE_CLIENT_SECRET", "op://test/token")
        ports = set()
        for service in GOOGLE_SERVICES:
            with self.subTest(service=service):
                self.launch(f"google-{service}")
                result = json.loads(capture.read_text())
                args = result["argv"]
                self.assertIn(f"https://{service}mcp.googleapis.com/mcp/v1", args)
                ports.add(args[3])
                self.assertNotIn("fixture-resolved-token", json.dumps(args))
                self.assertEqual(
                    result["env"]["GOOGLE_CLIENT_SECRET"],
                    "fixture-resolved-token",
                )
                client = json.loads(args[args.index("--static-oauth-client-info") + 1])
                self.assertEqual(client["client_secret"], "${GOOGLE_CLIENT_SECRET}")
        self.assertEqual(len(ports), len(GOOGLE_SERVICES))

    def test_search_and_x_load_separate_credentials(self) -> None:
        capture = self.install_mocks()
        self.credential("brave-search", "BRAVE_API_KEY", "fixture-brave-key")
        self.credential("x", "CLIENT_SECRET", "fixture-x-secret")
        for name, variable, value in (
            ("brave-search", "BRAVE_API_KEY", "fixture-brave-key"),
            ("x", "CLIENT_SECRET", "fixture-x-secret"),
        ):
            with self.subTest(server=name):
                self.launch(name)
                result = json.loads(capture.read_text())
                self.assertEqual(result["env"][variable], value)
                self.assertNotIn(value, json.dumps(result["argv"]))

    def test_google_callback_port_can_be_overridden_for_another_account(self) -> None:
        capture = self.install_mocks()
        self.credential("google-workspace", "GOOGLE_CLIENT_ID", "fixture-client-id")
        self.credential("google-workspace", "GOOGLE_CLIENT_SECRET", "op://test/token")
        self.credential("google-workspace", "GOOGLE_GMAIL_CALLBACK_PORT", "3435")
        self.launch("google-gmail")
        args = json.loads(capture.read_text())["argv"]
        self.assertIn("3435", args)
        self.credential("google-workspace", "GOOGLE_GMAIL_CALLBACK_PORT", "invalid")
        with self.assertRaises(subprocess.CalledProcessError):
            self.launch("google-gmail")

    def test_google_rejects_missing_credentials_and_unknown_service(self) -> None:
        launcher = str(self.config / "bin/mcp-google-workspace")
        for args in (["unknown"], ["gmail", "extra"], ["gmail"]):
            with (
                self.subTest(args=args),
                self.assertRaises(subprocess.CalledProcessError),
            ):
                self.run_command([launcher, *args])

    def assert_opencode_servers(self) -> None:
        rendered = json.loads((self.root / "opencode.json").read_text())
        opencode = rendered["mcp"]["servers"]
        self.assertEqual(set(opencode), set(self.servers))
        self.assertEqual(
            opencode["github"]["command"],
            [self.servers["github"]["command"], *self.servers["github"]["args"]],
        )
        self.assertIn("--isolated", opencode["playwright"]["command"])
        for name, server in self.servers.items():
            expected = "remote" if "url" in server else "local"
            self.assertEqual(opencode[name]["type"], expected)
            if "url" in server:
                self.assertEqual(opencode[name]["url"], server["url"])
            if "startup_timeout_sec" in server:
                self.assertEqual(
                    opencode[name]["timeout"]["startup"],
                    server["startup_timeout_sec"] * 1000,
                )

    def test_agent_sync_preserves_transport_and_browser_policy(self) -> None:
        claudius = os.environ.get("CLAUDIUS_BIN", "claudius")
        for agent in ("claude-code", "codex", "gemini", "opencode"):
            self.run_command([claudius, "config", "sync", "--agent", agent])
        codex = tomllib.loads((self.root / ".codex/config.toml").read_text())
        gemini = json.loads((self.root / ".gemini/settings.json").read_text())
        claude = json.loads((self.root / ".mcp.json").read_text())
        for rendered in (
            codex["mcp_servers"],
            gemini["mcpServers"],
            claude["mcpServers"],
        ):
            self.assertEqual(set(rendered), set(self.servers))
            self.assertEqual(rendered["github"]["args"], self.servers["github"]["args"])
            self.assertIn("--isolated", rendered["playwright"]["args"])
        self.assert_opencode_servers()
        policy = codex["mcp_servers"]["playwright"]
        for name, server in self.servers.items():
            if "url" in server:
                self.assertEqual(claude["mcpServers"][name]["type"], "http")
                self.assertEqual(gemini["mcpServers"][name]["type"], "http")
                self.assertEqual(codex["mcp_servers"][name]["url"], server["url"])
        self.assertTrue(set(policy["disabled_tools"]) >= UNSAFE_TOOLS)
        self.assertEqual(policy["default_tools_approval_mode"], "prompt")
        self.assertEqual(
            codex["mcp_servers"]["github"]["default_tools_approval_mode"],
            "writes",
        )
        self.assertEqual(codex["approval_policy"], "on-request")
        claude_settings = json.loads((self.root / ".claude/settings.json").read_text())
        self.assertTrue(
            {f"mcp__playwright__{t}" for t in UNSAFE_TOOLS}
            <= set(claude_settings["permissions"]["deny"]),
        )
        gemini_policy = tomllib.loads((SOURCE / "gemini.policy.toml").read_text())
        denied = {
            r["toolName"]
            for r in gemini_policy["rule"]
            if r.get("mcpName") == "playwright" and r["decision"] == "deny"
        }
        self.assertTrue(denied >= UNSAFE_TOOLS)

    def test_opencode_local_model_values_stay_out_of_tracked_settings(self) -> None:
        claudius = os.environ.get("CLAUDIUS_BIN", "claudius")
        self.run_command([claudius, "config", "sync", "--agent", "opencode"])
        rendered = json.loads((self.root / "opencode.json").read_text())
        provider = rendered["providers"]["llamacpp"]
        local_model = "{file:~/.config/opencode/local-model/"
        self.assertTrue(provider["settings"]["baseURL"].startswith(local_model))
        for model in provider["models"].values():
            self.assertTrue(model["modelID"].startswith(local_model))
            self.assertTrue(model["name"].startswith(local_model))
        self.assertEqual(rendered["model"], "llamacpp/local")
        self.assertNotIn("mcpServers", rendered)


if __name__ == "__main__":
    unittest.main()
