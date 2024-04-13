local mods = require("modifiers")
local mic = require("mic")
local audio = require("audio")
local window = require("window")

-----------------------------------------------
-- Bindings
-----------------------------------------------

-- Symbols layer
-----------------------------------------------

-- Emojis
hs.hotkey.bind("shift", "f13", function() hs.eventtap.keyStroke({"cmd", "ctrl"}, "space") end)


-- Super layer
-----------------------------------------------

-- Toggle Mic
mic.bind(mods.super, "t")

-- Switch Audio output
audio.bind(mods.super, "y")

-- U: Taken by 1Password mini
-- I: Taken by Alfred (Snippets)
-- O: Taken by Alfred (Clipboard History)
-- P: Taken by Alfred (Universal Action)

-- Window Nav
hs.hotkey.bind(mods.super, "k", hs.grid.show)
window.bindResize(mods.super, {"h", "l"})
hs.hotkey.bind(mods.super, "j", window.moveToNextScreen)

-- Lock screen
hs.hotkey.bind(mods.super, ";", hs.caffeinate.lockScreen)

hs.hotkey.bind(
  mods.super, "m",
  function()
    hs.application.launchOrFocus("MacGPT")
  end
)


-- Hyper layer: Launcher
-----------------------------------------------
-- NOTE Cannot map `,` and `.` because they are already bound to sysdiagnose at system level

hs.hotkey.bind(mods.hyper, "h", hs.reload)

-- Appps launcher
spoon.SpoonInstall:andUse("AppLauncher", {
    hotkeys = {
        a = "Figma",
        b = "Microsoft Edge",
        c = "Calendar",
        d = "Deepl",
        e = "Emacs",
        f = "Finder",
        g = "Arc",
        -- h = "", -- already bound to reload 🔨🥄
        -- i = "",
        -- j = "",
        k = "kitty",
        -- l = "",
        m = "Mail",
        n = "Obsidian",
        -- o = "",
        p = "Preview",
        q = "Activity Monitor",
        -- r = "",
        s = "Slack",
        t = "Microsoft To Do",
        u = "1Password",
        -- v = "",
        -- w = "",
        x = "Telegram",
        y = "Spotify",
        z = "zoom.us",
    },
    config = {
        modifiers = mods.hyper
    }
})
