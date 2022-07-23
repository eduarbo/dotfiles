local mods = require("modifiers")

spoon.SpoonInstall:andUse("AppLauncher", {
  hotkeys = {
    a = "Activity Monitor",
    b = "Firefox Developer Edition",
    c = "Calendar",
    d = "Deepl",
    e = "Emacs",
    f = "Finder",
    g = "Google Chrome",
    -- h = "",
    i = "kitty",
    -- j = "",
    -- k = "",
    -- l = "",
    m = "Mail",
    n = "Obsidian",
    -- o = "", Alfred
    p = "Spotify",
    -- q = "",
    r = "Telegram",
    s = "Slack",
    t = "Microsoft To Do",
    -- u = "", 1Password Quick Access
    v = "Visual Studio Code",
    w = "WhatsApp",
    -- x = "",
    y = "1Password",
    z = "zoom.us",
  },
  config = {
    modifiers = mods.meh
  }
})
