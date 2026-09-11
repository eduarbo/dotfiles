#!/usr/bin/env python3
"""Regression checks using OpenSSH's real config parser, no live agent/network."""
import os
from pathlib import Path
import socket
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[3]
CHECK = ROOT / "shell/ssh/bin/ssh-agent-check"

with tempfile.TemporaryDirectory(prefix="ssh-check-", dir="/tmp") as tmp:
    home = Path(tmp) / "home with spaces"
    home.mkdir()
    env = dict(os.environ, HOME=str(home))
    source = (ROOT / "shell/ssh/config").read_text()
    # OpenSSH expands ~ using passwd, not HOME. Make every fixture path explicit.
    config = home / "config"
    config.write_text(source.replace("~/", str(home) + "/").replace(
        f"IdentityAgent {home}/.bitwarden-ssh-agent.sock",
        f'IdentityAgent "{home}/.bitwarden-ssh-agent.sock"'))
    # Include paths with spaces also require quoting, even when absent.
    config.write_text("\n".join(
        f'Include "{line[8:]}"' if line.startswith("Include ") else line
        for line in config.read_text().splitlines()) + "\n")

    def check(path=config):
        return subprocess.run([str(CHECK), "--config", str(path)], env=env,
                              text=True, capture_output=True)

    assert check().returncode == 1, "missing socket passed"
    sock = socket.socket(socket.AF_UNIX)
    sock.bind(str(home / ".bitwarden-ssh-agent.sock"))
    try:
        result = check()
        assert result.returncode == 0, result.stderr
        assert "not verified" in result.stdout, "overclaimed authentication"
        effective = subprocess.check_output(
            ["ssh", "-G", "-F", str(config), "git@github.com"],
            text=True, stderr=subprocess.DEVNULL)
        assert "identitiesonly no\n" in effective
        assert "identityfile none\n" in effective

        blocked = home / "blocked"
        blocked.write_text(config.read_text().replace("IdentitiesOnly no", "IdentitiesOnly yes"))
        result = check(blocked)
        assert result.returncode == 1 and "blocks all agent keys" in result.stderr

        disabled = home / "disabled"
        disabled.write_text(config.read_text().replace(
            f'IdentityAgent "{home}/.bitwarden-ssh-agent.sock"', "IdentityAgent none"))
        assert "agent is disabled" in check(disabled).stderr

        invalid = home / "invalid"
        invalid.write_text("NotAnSSHOption yes\n")
        assert check(invalid).returncode == 1

        (home / "selected.pub").touch()
        selected = subprocess.check_output(
            ["ssh", "-G", "-F", str(config), "-o", "IdentitiesOnly=yes",
             "-i", str(home / "selected.pub"), "git@github.com"],
            text=True, stderr=subprocess.DEVNULL)
        assert "identitiesonly yes\n" in selected
        assert f"identityfile {home}/selected.pub\n" in selected
    finally:
        sock.close()

print("SSH agent config regression checks passed (no authentication)")
