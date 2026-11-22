"""Render this Mac's current Hyper bindings on the shared Piantor artwork."""

import os
import sys
from pathlib import Path

TOPIC = Path(__file__).resolve().parents[1]
QMK = Path(os.environ.get("QMK_USERSPACE", TOPIC.parents[2] / "qmk_userspace"))


def main():
    if not (QMK / "docs/cover/launcher.py").is_file():
        raise SystemExit("Set QMK_USERSPACE to the eduarbo/qmk_userspace checkout.")
    sys.dont_write_bytecode = True
    sys.path.insert(0, str(QMK / "docs"))
    from cover.launcher import hyper_keys
    from cover.render import draw, switches
    from draw_layout import read_keymap, reference_frames

    physical, layers = read_keymap()
    bindings = (TOPIC / "config/bindings.lua").read_text()
    layers["Hyper"] = hyper_keys(bindings, physical, layers["macOS base"])
    frame = next(frame for frame in reference_frames() if frame.layer == "Hyper")
    halves = switches()
    for mobile, name in [(False, "launcher.svg"), (True, "launcher-mobile.svg")]:
        (TOPIC / "docs" / name).write_text(
            draw(halves, physical, layers, frame, mobile) + "\n"
        )


if __name__ == "__main__":
    main()
