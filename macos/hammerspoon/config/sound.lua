local sound = {}

-- Switches the default audio output to the next available device in a predefined sequence.
-- Iterates through groups of audio devices, setting the first available device in each group as the default.
-- Supports two groups: Headphones and Speakers
function switchOutput()
    local outputMap = {
        { -- Earbuds
            {"AirPods Pro Femto", "ᖰᖳ AirPods Pro"},
        },
        { -- Wired Headphones
            {"AB13X USB Audio", "🎧 Headphones (USB)"},
            {"KM_B2 Digital Audio", "🎧 Headphones (USB)"}
        },
        { -- Speakers
            {"External Headphones", "🔊 Speakers (Jack port)"},
            {"NS- 20G", "🔊 Speakers (Bluetooth)"},
            {"SWITCH", "🔊 Speakers"},
            {"MacBook Pro Speakers", "💻 MacBook"}
        }
    }
    local currentDevice = hs.audiodevice.defaultOutputDevice()
    local nextDevice = findDeviceInMap(hs.audiodevice.findOutputByName, outputMap, currentDevice)

    if nextDevice then
        nextDevice.device:setDefaultOutputDevice()
        hs.notify.new({title = "Sound out: " .. nextDevice.label, withdrawAfter = 1}):send()
    end
end

-- Output

function findDeviceInMap(findFn, deviceMap, currentDevice)
    local currentOutput = hs.audiodevice.defaultOutputDevice()
    local deviceFound = false
    local nextDevice = nil
    local startIndex = 1  -- Start from the first group if current device is not found

    -- Determine in which group the current device is located
    for index, group in ipairs(deviceMap) do
        for _, item in ipairs(group) do
            if item[1] == currentOutput:name() then
                startIndex = index % #deviceMap + 1  -- Move to the next group in circular order
                deviceFound = true
                break
            end
        end
        if deviceFound then break end
    end

    -- Search for the next available device starting from startIndex
    local groupCount = #deviceMap
    for i = 0, groupCount - 1 do
        local groupIndex = (startIndex + i - 1) % groupCount + 1
        local group = deviceMap[groupIndex]
        for _, item in ipairs(group) do
            local name = item[1]
            local device = findFn(name)
            if device then
                nextDevice = {
                    name = name,
                    label = item[2],
                    device = device
                }
                return nextDevice  -- Return the next device if found
            end
        end
    end

    return nextDevice  -- Return nil if no device is found
end

-- Input

local inputArray = {}

inputArray["MacBook Pro Microphone"] = "Internal Microphone"

function switch_input()
    local inputMap = {
        {"MacBook Pro Microphone", "💻 Internal Microphone"}
    }
    local currentDevice = hs.audiodevice.defaultInputDevice()
    local nextDevice = findDeviceInMap(hs.audiodevice.findInputByName, inputMap, currentDevice)

    if nextDevice then
        nextDevice.device:setDefaultOutputDevice()
        hs.notify.new({title = "Sound in: " .. nextDevice.label, withdrawAfter = 1}):send()
    end
end

function sound.bind(mods, key)
    hs.hotkey.bind(mods, key, switchOutput)
end

return sound
