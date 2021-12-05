local label = require("labels")

-- Output

-- FIXME show it on the top-right corner
local current_output = hs.audiodevice.defaultOutputDevice()
local outputArray = {}

outputArray["External Headphones"] = "🎧"
outputArray["MacBook Pro Speakers"] = "💻"

function switch_output()
    for _, device in pairs(hs.audiodevice.allOutputDevices()) do
        if device and device ~= current_output and outputArray[device:name()] then
            label.new("Audio output: " .. outputArray[device:name()]):show(1)
            current_output = device
            device:setDefaultOutputDevice()
            break
        end
    end
end

-- Input

local current_input = hs.audiodevice.defaultInputDevice()
local inputArray = {}

inputArray["MacBook Pro Microphone"] = "Internal Microphone"

function switch_input()
    for _, device in pairs(hs.audiodevice.allInputDevices()) do
        if device and device ~= current and inputArray[device:name()] then
            label.new("Audio in: " .. inputArray[device:name()]):show(1)
            current = device
            device:setDefaultInputDevice()
            break
        end
    end
end

local audioOutputKey = "F14"
hs.hotkey.bind({}, audioOutputKey, switch_output)
