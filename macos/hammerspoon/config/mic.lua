--                                ░█▄█░▀█▀░█▀▀
--                                ░█░█░░█░░█░░
--                                ░▀░▀░▀▀▀░▀▀▀
--
-- When the mic is muted, hold the bound key to talk and release it to mute again.
-- You can toggle mute by double-tapping the bound key

local mods = require("modifiers")

local mic = {}
local doubleTap = false
local pushToTalk = false
local recentlyTapped = false
local darkmode_status =
    hs.osascript.applescript('tell application "System Events"\nreturn dark mode of appearance preferences\nend tell')

local function displayStatus()
    local mic = hs.audiodevice.defaultInputDevice()

    if mic:muted() then
        hs.notify.new({title = "🎙❌ Muted", withdrawAfter = 1}):send()
    else
        hs.notify.new({title = "🎙 on Air", withdrawAfter = 1}):send()
    end
end

local function updateMicStatus(muted)
    local mic = hs.audiodevice.defaultInputDevice()
    local muted = mic:muted()

    if muted then
        -- micMenubar:setIcon("./icons/mic-mute.pdf", true)
        micMenubar:setIcon("./icons/mic-filled-mute.pdf", true)
    else
        -- micMenubar:setIcon("./icons/mic.pdf", true)
        -- micMenubar:setIcon("./icons/mic-speaking.pdf", true)
        micMenubar:setIcon("./icons/mic-filled.pdf", true)
    end
end

local function toggleMic()
    local mic = hs.audiodevice.defaultInputDevice()
    mic:setMuted(not mic:muted())
    updateMicStatus()
end

local doubleTapTimer =
    hs.timer.delayed.new(
    0.3,
    function()
        recentlyTapped = false
    end
)

local function onKeyDown(event)
    if not pushToTalk then
        pushToTalk = true
        toggleMic()

        if recentlyTapped then
            displayStatus()
            doubleTap = true
            recentlyTapped = false
        else
            recentlyTapped = true
            doubleTapTimer:start()
        end
    end

    return true
end

local function onKeyUp(event)
    pushToTalk = false

    if doubleTap then
        doubleTap = false
    else
        toggleMic()
    end
end

if not micMenubar then
    micMenubar = hs.menubar.new()
end

function mic.bind(mods, key)
    hs.hotkey.bind(mods, key, onKeyDown, onKeyUp)
end

micMenubar:setClickCallback(toggleMic)
updateMicStatus()

return mic
