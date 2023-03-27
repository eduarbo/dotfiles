local mods = require("modifiers")
local layerMods = mods.hyper

-- Bindings for debugging
-- FIXME I tried to map these to "." and "," but it never worked
hs.hotkey.bind(layerMods, "h", hs.toggleConsole)
hs.hotkey.bind(layerMods, "r", hs.reload)

-- Appps launcher
spoon.SpoonInstall:andUse("AppLauncher", {
  hotkeys = {
    a = "Figma",
    b = "Google Chrome",
    c = "Calendar",
    d = "Deepl",
    e = "Emacs",
    f = "Finder",
    g = "Microsoft Edge Dev",
    -- i = "",
    -- j = "",
    k = "kitty",
    l = "Logseq",
    m = "Mail",
    n = "Obsidian",
    o = "Spotify",
    p = "Preview",
    q = "Activity Monitor",
    s = "Slack",
    t = "Microsoft To Do",
    u = "1Password",
    v = "Visual Studio Code",
    w = "WhatsApp",
    -- x = "",
    -- y = "",
    z = "zoom.us",
  },
  config = {
    modifiers = layerMods
  }
})
