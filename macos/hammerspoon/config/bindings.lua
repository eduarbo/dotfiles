local window = require("window")
local mods = require("modifiers")
local audio = require("audio")
local mic = require("mic")

local layerMods = mods.super

-- Window Nav
hs.hotkey.bind(layerMods, ";", hs.grid.show)
window.bindResize(layerMods, {"h", "j", "k", "l"})
hs.hotkey.bind(layerMods, "n", window.moveToNextScreen)

-- Emojis
hs.hotkey.bind({"shift"}, "f13", function() hs.eventtap.keyStroke({"cmd", "ctrl"}, "space") end)

-- Audio output switcher
audio.bind(layerMods, "y")

-- Mic toggle
mic.bind(layerMods, "t")
