local mods = require("modifiers")
local mic = require("mic")
local sound = require("sound")
local window = require("window")

-----------------------------------------------
-- Bindings
-----------------------------------------------

-- Symbols layer
-----------------------------------------------

-- Emojis
hs.hotkey.bind("shift", "f16", function() hs.eventtap.keyStroke({"cmd", "ctrl"}, "space") end)


-- Super layer
-----------------------------------------------

-- Toggle Mic
mic.bind(mods.super, "w")

-- Switch Sound output
sound.bind(mods.super, "y")

-- U: Taken by 1Password mini
-- I: Taken by ChatGPT
-- O: Taken by Alfred (Clipboard History)

-- Window Nav
hs.hotkey.bind(mods.super, "h", window.toggleLeftSize)
hs.hotkey.bind(mods.super, "j", window.moveToNextScreen)
hs.hotkey.bind(mods.super, "k", window.toggleMaximize)
hs.hotkey.bind(mods.super, "l", window.toggleRightSize)
hs.hotkey.bind(mods.super, ";", hs.grid.show)

-- M: Taken by Alfred to search for emojis
-- ,: Taken by Alfred (Snippets)
-- .: Taken by Alfred (Universal Action)

-- Lock screen
hs.hotkey.bind("cmd", "f16", hs.caffeinate.lockScreen)
hs.hotkey.bind("cmd", "escape", hs.caffeinate.lockScreen)

-- Hyper layer: Launcher
-----------------------------------------------
-- NOTE Cannot map `,` and `.` because they are already bound to sysdiagnose at system level

hs.hotkey.bind(mods.hyper, "h", hs.reload)

-- Appps launcher
spoon.SpoonInstall:andUse("AppLauncher", {
    hotkeys = {
        a = "Figma",
        b = "Arc",
        c = "Calendar",
        d = "Firefox",
        e = "Emacs",
        f = "Finder",
        g = "Brave Browser",
        -- h = "", -- already bound to reload 🔨🥄
        i = "ChatGPT",
        -- j = "",
        k = "kitty",
        l = "Logseq",
        m = "Mail",
        n = "Logseq",
        -- o = "",
        p = "Preview",
        q = "Activity Monitor",
        r = "Microsoft Teams",
        s = "Slack",
        t = "Microsoft To Do",
        u = "1Password",
        -- v = "",
        w = "WhatsApp",
        x = "Telegram",
        y = "YouTube Music",
        z = "zoom.us",
    },
    config = {
        modifiers = mods.hyper
    }
})
