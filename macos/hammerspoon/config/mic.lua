--                                ░█▄█░▀█▀░█▀▀
--                                ░█░█░░█░░█░░
--                                ░▀░▀░▀▀▀░▀▀▀
--
-- When the mic is muted hold f13 key to talk and release to mute again.
-- You can toggle mute by double tapping the f13 key

local utils = require("utils")
local label = require("labels")

local micKey = "F13"
local doubleTap = false
local pushToTalk = false
local recentlyTapped = false
local mic = hs.audiodevice.defaultInputDevice()
local darkmode_status =
    hs.osascript.applescript('tell application "System Events"\nreturn dark mode of appearance preferences\nend tell')

local function displayStatus()
    if mic:muted() then
        label.new("🎙 Muted"):show(1)
    else
        label.new("🎙 on Air"):show(1)
    end
end

local function updateMicStatus(muted)
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
    local muted = not mic:muted()
    mic:setMuted(muted)
    updateMicStatus(muted)
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

hs.hotkey.bind({}, micKey, onKeyDown, onKeyUp)

if not micMenubar then
    micMenubar = hs.menubar.new()
end
micMenubar:setClickCallback(toggleMic)
updateMicStatus(mic:muted())
