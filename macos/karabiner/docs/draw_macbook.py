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


def mobile_reference(deck, thumbs):
    mobile = element(
        "svg",
        width=470,
        height=490,
        viewBox="0 0 470 490",
        role="img",
        aria_label="MacBook typing area and tap and hold legend",
        font_family="Inter, Arial, sans-serif",
        fill="#142d31",
    )
    mobile.append(element("rect", width=470, height=490, rx=15, fill="#f6f0e2"))
    label(
        mobile,
        18,
        27,
        "KARABINER-ELEMENTS  /  macOS",
        13,
        font_family="Menlo, monospace",
    )
    label(mobile, 17, 66, "MacBook keycaps", 34, font_weight="750")
    scaled = element("g", transform="translate(0 25) scale(.49)")
    scaled.append(deepcopy(deck))
    mobile.append(scaled)
    for x, title in [(19, "Printed key"), (191, "Tap"), (330, "Hold")]:
        label(mobile, x, 244, title, 15, font_weight="650", fill="#41595b")
    for i, (name, tap, hold) in enumerate(thumbs):
        y = 257 + i * 42
        mobile.append(element("path", d=f"M18 {y}h434", stroke="#bec7b9"))
        label(mobile, 19, y + 26, name, 18, font_weight="650")
        label(mobile, 191, y + 26, tap, 18)
        label(mobile, 330, y + 26, hold, 18, font_weight="650")
    ET.indent(mobile)
    (ROOT / "docs/macbook-mobile.svg").write_text(
        ET.tostring(mobile, encoding="unicode") + "\n"
    )


def draw():
    generated = check_output(
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
    raw = ET.fromstring(generated)
    canvas = element(
        "svg",
        width=960,
        height=463,
        viewBox="0 0 960 463",
        role="img",
        aria_labelledby="title desc",
        font_family="Inter, Arial, sans-serif",
        fill="#142d31",
    )
    for tag, value in [
        ("title", "MacBook keycaps · Karabiner-Elements"),
        (
            "desc",
            "Typing area of the ANSI MacBook. The two Option keys, two Command keys and space bar do the work of six split thumbs.",
        ),
    ]:
        child = element(tag, id=tag)
        child.text = value
        canvas.append(child)
    canvas.append(element("rect", width=960, height=463, rx=19, fill="#f6f0e2"))
    label(
        canvas,
        30,
        31,
        "KARABINER-ELEMENTS  /  macOS",
        13,
        font_family="Menlo, monospace",
        fill="#41595b",
        font_weight="650",
    )
    label(canvas, 29, 77, "MacBook keycaps", 42, font_weight="750")
    label(
        canvas,
        930,
        71,
        "Printed key · tap · hold",
        19,
        text_anchor="end",
        fill="#41595b",
    )
    canvas.append(element("path", d="M30 98H930 M30 401H930", stroke="#b2bcb1"))
    # The board group has its own local coordinate system, also used by the gesture tutorial.
    deck = element("g", id="board")
    canvas.append(deck)
    deck.append(
        element("rect", x=30, y=123, width=900, height=261, rx=17, fill="#adb1a7")
    )
    deck.append(
        element("rect", x=30, y=118, width=900, height=259, rx=17, fill="#334d50")
    )
    keys = element("g", transform="translate(45 128)")
    deck.append(keys)
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
        keys.append(key)
        face = group.find(f"{{{NS}}}rect")
        x, y, w, h = (float(face.get(k)) for k in ("x", "y", "width", "height"))
        color = (
            "#cdbaff"
            if hold in {"Symbols", "Super"}
            else "#9ee6ca"
            if printed
            else "#fff8e8"
        )
        key.append(
            element("rect", x=x, y=y + 3, width=w, height=h, rx=6, fill="#142d31")
        )
        key.append(
            element("rect", x=x, y=y, width=w, height=h - 1, rx=6, fill="#94a29a")
        )
        key.append(
            element(
                "rect",
                x=x + 1,
                y=y,
                width=w - 2,
                height=h - 4,
                rx=5,
                fill=color,
                **{"class": "face"},
            )
        )
        if printed:
            label(key, 0, -14, printed, 11.5, font_weight="550")
        label(key, 0, 5, tap, 17 if len(tap) > 2 else 21, font_weight="700")
        if hold:
            label(key, 0, 19, hold, 11.5, font_weight="650")
            if printed == "space":
                name = "Space bar"
            else:
                side = "Left" if hold in {"Super", "Shift"} else "Right"
                name = f"{side} {printed.title()}"
            thumbs.append((name, tap, hold))
    mobile_reference(deck, thumbs)
    canvas.append(
        element("rect", x=30, y=423, width=6, height=19, rx=3, fill="#34776b")
    )
    label(
        canvas,
        48,
        439,
        "Hold right Command for Symbols. Tap it for Tab.",
        17,
        font_weight="650",
    )
    ET.indent(canvas)
    (ROOT / "docs/macbook.svg").write_text(
        ET.tostring(canvas, encoding="unicode") + "\n"
    )


if __name__ == "__main__":
    draw()
