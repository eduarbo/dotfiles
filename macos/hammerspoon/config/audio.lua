local log = require("log")

-- Output

function findDeviceInMap(findFn, deviceMap, currentDevice)
    local currentOutput = hs.audiodevice.defaultOutputDevice()
    local hasFoundCurrentDeviceOnMap = false
    local nextDevice

    for _, item in ipairs(deviceMap) do
        local name = item[1]
        local label = item[2]
        local isCurrentDevice = name == currentOutput:name()
        local device = findFn(name)

        if isCurrentDevice then
            hasFoundCurrentDeviceOnMap = true
        elseif device and (not nextDevice or hasFoundCurrentDeviceOnMap) then
            nextDevice = {}
            nextDevice['name'] = item[1]
            nextDevice['label'] = item[2]
            nextDevice['device'] = device

            if hasFoundCurrentDeviceOnMap then
                break
            end
        end
    end

    return nextDevice
end

function switchOutput()
    local outputMap = {
        {"External Headphones", "🎧 Headphones"},
        {"MacBook Pro Speakers", "💻 MacBook"},
        {"LG HDR QHD", "🔊 Speakers"}
    }
    local currentDevice = hs.audiodevice.defaultOutputDevice()
    local nextDevice = findDeviceInMap(hs.audiodevice.findOutputByName, outputMap, currentDevice)

    if nextDevice then
        nextDevice.device:setDefaultOutputDevice()
        hs.notify.new({ title = "Audio out: " .. nextDevice.label, withdrawAfter = 1 }):send()
    end
end

local audioOutputKey = "F14"
hs.hotkey.bind({}, audioOutputKey, switchOutput)


-- Input

local inputArray = {}

inputArray["MacBook Pro Microphone"] = "Internal Microphone"

function switch_input()
    local inputMap = {
        {"MacBook Pro Microphone", "💻 Internal Microphone"},
    }
    local currentDevice = hs.audiodevice.defaultInputDevice()
    local nextDevice = findDeviceInMap(hs.audiodevice.findInputByName, inputMap, currentDevice)

    if nextDevice then
        nextDevice.device:setDefaultOutputDevice()
        hs.notify.new({ title = "Audio in: " .. nextDevice.label, withdrawAfter = 1 }):send()
    end
end
