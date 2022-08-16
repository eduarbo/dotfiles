local mods = require("modifiers")
local layerMods = mods.super

-- Bindings for debugging
hs.hotkey.bind(layerMods, "/", hs.toggleConsole)
hs.hotkey.bind(layerMods, ".", hs.reload)

-- Appps launcher
spoon.SpoonInstall:andUse("AppLauncher", {
  hotkeys = {
    a = "Activity Monitor",
    b = "Finder",
    c = "Calendar",
    d = "Deepl",
    e = "Emacs",
    f = "Firefox Developer Edition",
    g = "Google Chrome",
    -- h = "",
    i = "kitty",
    -- j = "",
    -- k = "",
    -- l = "",
    m = "Mail",
    n = "Obsidian",
    -- o = ""
    p = "Spotify",
    -- q = "",
    r = "Telegram",
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
