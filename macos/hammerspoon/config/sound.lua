local sound = {}

local function findNextDevice(findFn, deviceMap, currentDevice)
    local deviceFound = false
    local startIndex = 1

    for index, group in ipairs(deviceMap) do
        for _, item in ipairs(group) do
            if item[1] == currentDevice:name() then
                startIndex = index % #deviceMap + 1
                deviceFound = true
                break
            end
        end
        if deviceFound then break end
    end

    local groupCount = #deviceMap
    for i = 0, groupCount - 1 do
        local groupIndex = (startIndex + i - 1) % groupCount + 1
        local group = deviceMap[groupIndex]
        for _, item in ipairs(group) do
            local device = findFn(item[1])
            if device then
                return { name = item[1], label = item[2], device = device }
            end
        end
    end

    return nil
end

-- Switches the default audio output to the next available device in a predefined sequence.
-- Iterates through groups of audio devices, setting the first available device in each group as the default.
-- Supports two groups: Headphones and Speakers
local function switchOutput()
    local outputMap = {
        {
            {"AB13X USB Audio", "🎧 Headphones (USB)"},
        },
        {
            {"KM_B2 Digital Audio", "🎧 Headphones (USB)"},
        },
        {
            {"External Headphones", "🔊 External (Jack port)"},
        },
        {
            {"AirPods Pro Femto", "ᖰᖳ AirPods Pro"},
        },
        {
            {"Chat-Audeze Maxwell", "🎧 Audeze Maxwell"},
        },
        {
            {"NS- 20G", "🔊 Speakers (Bluetooth)"},
            {"SWITCH", "🔊 Speakers"},
            {"MacBook Pro Speakers", "💻 MacBook"}
        }
    }
    local currentDevice = hs.audiodevice.defaultOutputDevice()
    local nextDevice = findNextDevice(hs.audiodevice.findOutputByName, outputMap, currentDevice)

    if nextDevice then
        nextDevice.device:setDefaultOutputDevice()
        hs.notify.new({title = "Sound out: " .. nextDevice.label, withdrawAfter = 1}):send()
    end
end

function sound.bind(mods, key)
    hs.hotkey.bind(mods, key, switchOutput)
end

return sound
