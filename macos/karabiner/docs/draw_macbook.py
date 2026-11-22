"""Render the MacBook reference from the project's keymap-drawer geometry and legends."""

from copy import deepcopy
from pathlib import Path
from subprocess import check_output
from xml.etree import ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
NS = "http://www.w3.org/2000/svg"
ET.register_namespace("", NS)


def element(tag, **attrs):
    return ET.Element(
        f"{{{NS}}}{tag}", {k.replace("_", "-"): str(v) for k, v in attrs.items()}
    )


def label(parent, x, y, value, size, **kwargs):
    node = element("text", x=x, y=y, font_size=size, **kwargs)
    node.text = value
    parent.append(node)


def legend(group, kind):
    for node in group.findall(f"{{{NS}}}text"):
        if kind in node.get("class", "").split():
            return "".join(node.itertext()).strip()
    return ""


DARK, FACE, INK, MINT, MUTED = "#193132", "#f6f0e0", "#183131", "#a3eccb", "#bdd0c8"
FONT = "Inter, Arial, sans-serif"
MONO = "IBM Plex Mono, Menlo, monospace"


def write_reference(board, thumbs, mobile):
    width, height = (520, 550) if mobile else (1200, 451)
    pad = 23 if mobile else 34
    svg = element(
        "svg",
        width=width,
        height=height,
        viewBox=f"0 0 {width} {height}",
        role="img",
        aria_labelledby="title desc",
        font_family=FONT,
        fill=INK,
    )
    for tag, content in [
        ("title", "MacBook · base"),
        (
            "desc",
            "Printed labels, tap actions and holds on the ANSI MacBook typing area.",
        ),
    ]:
        child = element(tag, id=tag)
        child.text = content
        svg.append(child)
    svg.append(element("rect", width=width, height=height, fill=DARK))
    label(
        svg,
        pad,
        32,
        "MACBOOK   /   KARABINER",
        16,
        fill=MUTED,
        font_family=MONO,
        font_weight=650,
    )
    label(
        svg, pad, 79, "Base · macOS", 44 if mobile else 49, fill=FACE, font_weight=720
    )
    if mobile:
        label(svg, pad, 117, "Printed key · tap · hold", 23, fill=MINT, font_weight=600)
    else:
        label(
            svg,
            width - pad,
            72,
            "Printed key · tap · hold",
            29,
            fill=MINT,
            font_weight=600,
            text_anchor="end",
        )
    holder = element(
        "g",
        transform=(
            "translate(18 145) scale(.555)"
            if mobile
            else "translate(34 116) scale(1.3)"
        ),
    )
    holder.append(deepcopy(board))
    svg.append(holder)
    if mobile:
        for x, title in [(23, "Printed key"), (213, "Tap"), (363, "Hold")]:
            label(svg, x, 319, title, 17, fill=MUTED, font_weight=600)
        for i, (name, tap, hold) in enumerate(thumbs):
            y = 355 + i * 43
            label(svg, 23, y, name, 22, fill=FACE, font_weight=650)
            label(svg, 213, y, tap, 22, fill=FACE)
            label(svg, 363, y, hold, 22, fill=FACE, font_weight=650)
    ET.indent(svg)
    dest = ROOT / "docs" / ("macbook-mobile.svg" if mobile else "macbook.svg")
    dest.write_text(ET.tostring(svg, encoding="unicode") + "\n")


def draw():
    raw = ET.fromstring(
        check_output(
            [
                "uvx",
                "--from",
                "keymap-drawer==0.23.0",
                "keymap",
                "draw",
                "docs/macbook.yaml",
            ],
            cwd=ROOT,
        )
    )
    board = element("g", id="board", font_family=FONT, fill=INK)
    thumbs = []
    for group in raw.iter(f"{{{NS}}}g"):
        if "keypos-" not in group.get("class", ""):
            continue
        tap, hold, printed = (legend(group, k) for k in ("tap", "hold", "shifted"))
        keypos = next(
            t[7:] for t in group.get("class").split() if t.startswith("keypos-")
        )
        key = element(
            "g",
            id=f"key-{keypos}",
            transform=group.get("transform"),
            data_position=printed or tap,
            data_hold=hold,
            text_anchor="middle",
        )
        board.append(key)
        face = group.find(f"{{{NS}}}rect")
        x, y, w, h = (float(face.get(k)) for k in ("x", "y", "width", "height"))
        key.append(
            element("rect", x=x, y=y + 2.5, width=w, height=h, rx=6, fill="#0e2426")
        )
        key.append(
            element(
                "rect",
                x=x,
                y=y,
                width=w,
                height=h - 1,
                rx=6,
                fill=FACE if printed else "#d7dfd3",
                **{"class": "face"},
            )
        )
        if printed:
            label(key, 0, -14, printed, 12, font_weight=600)
        label(
            key, 0, 4 if hold else 7, tap, 18 if len(tap) > 2 else 22, font_weight=700
        )
        if hold:
            label(key, 0, 20, hold, 13, font_weight=650)
            side = "Left" if hold in {"Super", "Shift"} else "Right"
            thumbs.append(
                (
                    "Space bar" if printed == "space" else f"{side} {printed.title()}",
                    tap,
                    hold,
                )
            )
    for mobile in (False, True):
        write_reference(board, thumbs, mobile)


if __name__ == "__main__":
    draw()
