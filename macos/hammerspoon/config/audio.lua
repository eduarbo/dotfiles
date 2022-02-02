-- Output

local current_output = hs.audiodevice.defaultOutputDevice()
local outputArray = {}

outputArray["External Headphones"] = "🎧 Headphones"
outputArray["MacBook Pro Speakers"] = "💻 MacBook"
outputArray["LG HDR QHD"] = "🔊 Speakers"

function switch_output()
    for _, device in pairs(hs.audiodevice.allOutputDevices()) do
        if device and device ~= current_output and outputArray[device:name()] then
            hs.notify.new({ title = "Audio output: " .. outputArray[device:name()], withdrawAfter = 1 }):send()
            current_output = device
            device:setDefaultOutputDevice()
            break
        end
    end
end

local audioOutputKey = "F14"
hs.hotkey.bind({}, audioOutputKey, switch_output)


-- Input

local current_input = hs.audiodevice.defaultInputDevice()
local inputArray = {}

inputArray["MacBook Pro Microphone"] = "Internal Microphone"

function switch_input()
    for _, device in pairs(hs.audiodevice.allInputDevices()) do
        if device and device ~= current and inputArray[device:name()] then
            hs.notify.new({ title = "Audio in: " .. inputArray[device:name()], withdrawAfter = 1 }):send()
            current = device
            device:setDefaultInputDevice()
            break
        end
    end
end
