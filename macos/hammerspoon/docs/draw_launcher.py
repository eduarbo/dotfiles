#!/usr/bin/env python3
"""Draw docs/launcher.svg from the Hyper app launcher in config/bindings.lua.

Run `python3 docs/draw_launcher.py` from this topic. It needs `uvx` to run a pinned
keymap-drawer.
"""

import json
import os
import re
import subprocess
import tempfile
from pathlib import Path

KEYMAP_DRAWER = "keymap-drawer==0.23.0"
TOPIC = Path(__file__).resolve().parent.parent
BINDINGS = TOPIC / "config/bindings.lua"
OUTPUT = TOPIC / "docs/launcher.svg"

QWERTY = ["qwert", "yuiop", "asdfg", "hjkl;", "zxcvb", "nm,./"]


def hyper_bindings(source: str) -> dict[str, str]:
    """Map each Hyper letter to what it opens or does."""
    labels: dict[str, str] = {}
    if launcher := re.search(r'"AppLauncher".*?hotkeys\s*=\s*\{(.*?)\}', source, re.S):
        for line in launcher.group(1).splitlines():
            if app := re.match(r'\s*(\w)\s*=\s*"([^"]+)"', line):
                labels[app.group(1)] = app.group(2)
            elif taken := re.match(r'\s*--\s*(\w)\s*=\s*"",\s*--\s*already bound to (.+?)(?: in Raycast)?\s*$', line):
                labels.setdefault(taken.group(1), taken.group(2)[:1].upper() + taken.group(2)[1:])
    for key in re.findall(r'hs\.hotkey\.bind\(mods\.hyper,\s*"(\w)",\s*hs\.reload\)', source):
        labels[key] = "Reload"
    return labels


def main() -> None:
    labels = hyper_bindings(BINDINGS.read_text())
    keys = []
    for row in QWERTY:
        for key in row:
            name = labels.get(key)
            keys.append({"t": name.replace(" ", "\n"), "s": key.upper()} if name else {"t": "", "s": key.upper(), "type": "ghost"})

    keymap = {
        "layout": {"ortho_layout": {"split": True, "rows": 3, "columns": 5, "thumbs": 0}},
        "draw_config": {
            "dark_mode": "auto",
            "key_w": 76,
            "key_h": 60,
            "shrink_wide_legends": 7,
            "outer_pad_h": 20,
            "svg_extra_style": "svg.keymap { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif; } text.label { display: none; }",
        },
        "layers": {"Hyper": keys},
    }
    with tempfile.NamedTemporaryFile("w", suffix=".yaml", delete=False) as handle:
        json.dump(keymap, handle, ensure_ascii=False)
    try:
        subprocess.run(["uvx", "--from", KEYMAP_DRAWER, "keymap", "draw", handle.name, "-o", OUTPUT], check=True)
    finally:
        os.unlink(handle.name)
    print(f"Wrote {OUTPUT.relative_to(TOPIC)} with {len(labels)} bindings")


if __name__ == "__main__":
    main()
