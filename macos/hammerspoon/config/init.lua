--                ░█░█░█▀█░█▄█░█▄█░█▀▀░█▀▄░█▀▀░█▀█░█▀█░█▀█░█▀█
--                ░█▀█░█▀█░█░█░█░█░█▀▀░█▀▄░▀▀█░█▀▀░█░█░█░█░█░█
--                ░▀░▀░▀░▀░▀░▀░▀░▀░▀▀▀░▀░▀░▀▀▀░▀░░░▀▀▀░▀▀▀░▀░▀

local log = require("log")
local mods = require("modifiers")
local mic = require("mic")
local audio = require("audio")
local window = require("window")

-- Manage Spoons
hs.loadSpoon("SpoonInstall")
spoon.SpoonInstall.use_syncinstall = true

log.i("Initializing")

-- Reload config on write
hs.pathwatcher.new(hs.configdir, hs.reload):start()
hs.notify.new({title = "Config reloaded 🔨🥄", withdrawAfter = 1}):send()

-- Alert styling
hs.alert.defaultStyle.strokeColor = {white = 0, alpha = 0}
hs.alert.defaultStyle.fillColor = {white = 0, alpha = 0.75}
hs.alert.defaultStyle.textStyle = {paragraphStyle = {alignment = "center"}}
hs.alert.defaultStyle.textSize = 25

-----------------------------------------------
-- Bindings
-----------------------------------------------

-- Sane defaults
-----------------------------------------------

require("press-twice-to-quit")


-- Symbols layer
-----------------------------------------------

-- Emojis
hs.hotkey.bind(mods.super, "n", function() hs.eventtap.keyStroke({"cmd", "ctrl"}, "space") end)


-- Super layer
-----------------------------------------------

-- Toggle Mic
mic.bind(mods.super, "t")

-- Switch Audio output
audio.bind(mods.super, "y")

-- U: Taken by 1Password mini
-- I: Taken by Alfred (Snippets)
-- O: Taken by Alfred
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
        -- o = "", -- Taken by Spotlight
        p = "Preview",
        q = "Activity Monitor",
        -- r = "",
        s = "Slack",
        t = "Microsoft To Do",
        u = "1Password",
        -- v = "",
        w = "WhatsApp",
        x = "Telegram",
        y = "Spotify",
        z = "zoom.us",
    },
    config = {
        modifiers = mods.hyper
    }
})
