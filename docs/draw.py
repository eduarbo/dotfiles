#!/usr/bin/env python3
"""Draw docs/lifecycle.svg, what `dot TOPIC` does, with Graphviz.

Run `python3 docs/draw.py` from anywhere; it needs Graphviz (`brew install graphviz`).
"""

import shutil
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DOCS = ROOT / "docs"

# Colors readable on GitHub's light and dark themes.
BLUE, PURPLE, GRAY = "#0969da", "#8250df", "#8b949e"
FONT = "Helvetica"

STYLE = f"""
  graph [bgcolor=transparent, fontname="{FONT}", fontsize=12, fontcolor="{GRAY}", pad=0.2];
  node  [fontname="{FONT}", fontsize=11, fontcolor=white, style="rounded,filled", shape=box,
         color=none, fillcolor="{BLUE}", margin="0.14,0.05", height=0.32];
  edge  [color="{GRAY}", fontcolor="{GRAY}", fontname="{FONT}", fontsize=10, arrowsize=0.6];
"""


LIFECYCLE = f"""digraph lifecycle {{
  rankdir=LR; nodesep=0.3; ranksep=0.35;
{STYLE}
  start   [label="dot TOPIC", shape=box, style="rounded", color="{GRAY}", fontcolor="{GRAY}"];
  os      [label="Runs on\\nthis OS?", shape=diamond, fillcolor="{PURPLE}", margin="0.02"];
  deps    [label="Install missing\\ndependencies first"];
  enabled [label="Already\\nenabled?", shape=diamond, fillcolor="{PURPLE}", margin="0.02"];
  link    [label="Link into\\n~/.local/share/dotfiles\\nthen _init link"];
  install [label="_init install"];
  update  [label="_init update"];
  skip    [label="Skipped", style="rounded", color="{GRAY}", fontcolor="{GRAY}"];
  undo    [label="Disabled again", style="rounded", color="{GRAY}", fontcolor="{GRAY}"];

  start -> os;
  os -> deps [label="yes"];
  os -> skip [label="no"];
  deps -> enabled;
  enabled -> link [label="no"];
  enabled -> update [label="yes"];
  link -> install;
  install -> undo [label="fails"];
}}"""


def render(source: str, name: str) -> None:
    graphviz = shutil.which("dot")
    if not graphviz:
        raise SystemExit("Graphviz is missing: brew install graphviz")
    subprocess.run([graphviz, "-Tsvg", "-o", DOCS / name], input=source, text=True, check=True)
    print(f"Wrote docs/{name}")


if __name__ == "__main__":
    render(LIFECYCLE, "lifecycle.svg")
