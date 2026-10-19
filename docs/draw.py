#!/usr/bin/env python3
"""Draw the diagrams in docs/ with Graphviz.

- topics.svg: every topic grouped by category, with the dependencies that each
  `_init deps` reports, so it always matches what `dot` installs.
- lifecycle.svg: what `dot TOPIC` does.

Run `python3 docs/draw.py` from anywhere; it needs Graphviz (`brew install graphviz`).
"""

import os
import shutil
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DOCS = ROOT / "docs"

# Colors readable on GitHub's light and dark themes.
BLUE, PURPLE, GRAY = "#0969da", "#8250df", "#8b949e"
FONT = "Helvetica"
CATEGORIES = {"macos": "macOS", "shell": "Shell", "editor": "Editors", "dev": "Development"}

STYLE = f"""
  graph [bgcolor=transparent, fontname="{FONT}", fontsize=12, fontcolor="{GRAY}", pad=0.2];
  node  [fontname="{FONT}", fontsize=11, fontcolor=white, style="rounded,filled", shape=box,
         color=none, fillcolor="{BLUE}", margin="0.14,0.05", height=0.32];
  edge  [color="{GRAY}", fontcolor="{GRAY}", fontname="{FONT}", fontsize=10, arrowsize=0.6];
"""


def init_query(topic: str, what: str) -> list[str]:
    env = {**os.environ, "DOTFILES": str(ROOT), "TOPIC": topic}
    result = subprocess.run(["./_init", what], cwd=ROOT / topic, env=env, capture_output=True, text=True)
    return result.stdout.split()


def topics_dot() -> str:
    topics = sorted(str(p.parent.relative_to(ROOT)) for p in ROOT.glob("*/*/_init") if os.access(p, os.X_OK))
    lines = ["digraph topics {", "  rankdir=LR; newrank=true; concentrate=true; nodesep=0.18; ranksep=0.6;", STYLE]
    for category, title in CATEGORIES.items():
        lines.append(f'  subgraph cluster_{category} {{ label="{title}"; labeljust=l; style="rounded,dashed"; color="{GRAY}";')
        for topic in topics:
            if topic.startswith(category + "/"):
                macos_only = init_query(topic, "os") == ["macos"]
                fill = f', fillcolor="{PURPLE}"' if macos_only else ""
                lines.append(f'    "{topic}" [label="{topic.split("/")[1]}"{fill}];')
        lines.append("  }")
    for topic in topics:
        for dep in init_query(topic, "deps"):
            lines.append(f'  "{topic}" -> "{dep}";')
    lines.append("}")
    return "\n".join(lines)


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
    render(topics_dot(), "topics.svg")
    render(LIFECYCLE, "lifecycle.svg")
