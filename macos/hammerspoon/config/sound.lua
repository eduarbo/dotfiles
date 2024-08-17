local sound = {}

-- Output

function findDeviceInMap(findFn, deviceMap, currentDevice)
    local currentOutput = hs.audiodevice.defaultOutputDevice()
    local hasFoundCurrentDeviceOnMap = false
    local nextDevice

    for _, group in ipairs(deviceMap) do
        if nextDevice then
            break
        end

        for _, item in ipairs(group) do
            local name = item[1]
            local label = item[2]
            local device = findFn(name)
            local isCurrentDevice = name == currentOutput:name()

            -- if current device is found in this group go to the next one
            if isCurrentDevice then
                hasFoundCurrentDeviceOnMap = true
                break
            elseif device and (not nextDevice or hasFoundCurrentDeviceOnMap) then
                nextDevice = {}
                nextDevice["name"] = name
                nextDevice["label"] = label
                nextDevice["device"] = device

                if hasFoundCurrentDeviceOnMap then
                    break
                end
            end
        end
    end

    return nextDevice
end

function switchOutput()
    local outputMap = {
        {{"AirPods Pro de Eduardo", "🎧 AirPods Pro"}, {"AB13X USB Audio", "🎧 Headphones (USB)"}, {"USB Audio Device", "🔊 Speakers (USB)"}},
        {{"External Headphones", "🔊 Speakers (Jack port)"}, {"NS- 20G", "🔊 Speakers (Bluetooth)"}, {"SWITCH", "🔊 Speakers"}, {"LG HDR QHD", "🔊 Speakers (HDMI)"}, {"MacBook Pro Speakers", "💻 MacBook"}}
    }
    local currentDevice = hs.audiodevice.defaultOutputDevice()
    local nextDevice = findDeviceInMap(hs.audiodevice.findOutputByName, outputMap, currentDevice)

    if nextDevice then
        nextDevice.device:setDefaultOutputDevice()
        hs.notify.new({title = "Sound out: " .. nextDevice.label, withdrawAfter = 1}):send()
    end
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
