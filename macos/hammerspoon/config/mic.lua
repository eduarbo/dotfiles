--                                ░█▄█░▀█▀░█▀▀
--                                ░█░█░░█░░█░░
--                                ░▀░▀░▀▀▀░▀▀▀
--
-- Push-to-talk: hold the bound key to unmute, release to mute again.
-- Double-tap toggles mute permanently.

local log = hs.logger.new("mic", "info")

local mic = {}
local doubleTap = false
local pushToTalk = false
local recentlyTapped = false

local iconPath = hs.configdir .. "/icons/"
local trackedDevice = nil

-- Use inputMuted/setInputMuted to avoid reading output mute on combo devices
local function getDevice()
    return hs.audiodevice.defaultInputDevice()
end

local function isMuted()
    local dev = getDevice()
    if not dev then return true end
    local m = dev:inputMuted()
    if m == nil then m = dev:muted() end
    return m == true
end

local function setMuted(state)
    local dev = getDevice()
    if not dev then return end
    if not dev:setInputMuted(state) then
        dev:setMuted(state)
    end
end

local function updateIcon()
    if not micMenubar then return end
    if isMuted() then
        micMenubar:setIcon(iconPath .. "mic-filled-mute.pdf", true)
    else
        micMenubar:setIcon(iconPath .. "mic-filled.pdf", true)
    end
end

local function displayStatus()
    if isMuted() then
        hs.notify.new({title = "🎙❌ Muted", withdrawAfter = 1}):send()
    else
        hs.notify.new({title = "🎙 on Air", withdrawAfter = 1}):send()
    end
end

local function toggleMic()
    setMuted(not isMuted())
    updateIcon()
end

-- Per-device watcher: fires on mute/volume/jack changes from any source
local function startDeviceWatcher()
    if trackedDevice then
        pcall(function() trackedDevice:watcherStop() end)
        trackedDevice = nil
    end

    local dev = getDevice()
    if not dev then return end

    dev:watcherCallback(function(uid, event)
        if event == "mute" then
            updateIcon()
        end
    end)
    dev:watcherStart()
    trackedDevice = dev
    log.i("Tracking mic: " .. dev:name())
end

-- System watcher: fires when default input device changes or devices appear/disappear
hs.audiodevice.watcher.setCallback(function(event)
    if event == "dIn" or event == "dev#" then
        log.i("Audio device event: " .. event)
        startDeviceWatcher()
        updateIcon()
    end
end)
hs.audiodevice.watcher.start()

-- Safety net: poll every 2s to catch anything the watchers might miss
local pollTimer = hs.timer.doEvery(2, updateIcon)

-- Double-tap detection
local doubleTapTimer = hs.timer.delayed.new(0.3, function()
    recentlyTapped = false
end)

local function onKeyDown()
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

local function onKeyUp()
    pushToTalk = false
    if doubleTap then
        doubleTap = false
    else
        toggleMic()
    end
end

-- Menubar
if not micMenubar then
    micMenubar = hs.menubar.new()
end

function mic.bind(mods, key)
    hs.hotkey.bind(mods, key, onKeyDown, onKeyUp)
end

micMenubar:setClickCallback(toggleMic)

-- Initialize: attach device watcher and set correct icon
startDeviceWatcher()
updateIcon()

return mic
