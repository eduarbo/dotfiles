--                ░█░█░█▀█░█▄█░█▄█░█▀▀░█▀▄░█▀▀░█▀█░█▀█░█▀█░█▀█
--                ░█▀█░█▀█░█░█░█░█░█▀▀░█▀▄░▀▀█░█▀▀░█░█░█░█░█░█
--                ░▀░▀░▀░▀░▀░▀░▀░▀░▀▀▀░▀░▀░▀▀▀░▀░░░▀▀▀░▀▀▀░▀░▀

local log = require("log")
local mods = require("modifiers")

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

--------------{ "cmd", "ctrl", "alt" }---------------------------------
-- Bindings for debugging
-----------------------------------------------
hs.hotkey.bind(mods.meh, "/", hs.toggleConsole)
hs.hotkey.bind(mods.meh, ".", hs.reload)

-----------------------------------------------
-- Modules
-----------------------------------------------

-- require("date-battery")
require("press-twice-to-quit")
require("mic")
require("netspeed")
require("window")
require("audio")
require("launcher")
-- require("cheatsheet")
