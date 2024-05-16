--                 ░█░█░▀█▀░█▀█░░░█▄█░█▀█░█▀█░█▀█░█▀▀░█▀▀░█▀▄
--                 ░█▄█░░█░░█░█░░░█░█░█▀█░█░█░█▀█░█░█░█▀▀░█▀▄
--                 ░▀░▀░▀▀▀░▀░▀░░░▀░▀░▀░▀░▀░▀░▀░▀░▀▀▀░▀▀▀░▀░▀
local mods = require("modifiers")

local window = {}

hs.window.animationDuration = 0

-- Ignore the misbehaving apps to supress warnings
hs.window.filter.ignoreAlways["WhatsApp Helper"] = true
hs.window.filter.ignoreAlways["Mail Networking"] = true
hs.window.filter.ignoreAlways["Firefox Developer EditionCP WebExtensions"] = true
hs.window.filter.ignoreAlways["Firefox Developer EditionCP Web Content"] = true
hs.window.filter.ignoreAlways["FirefoxCP Web Content"] = true
hs.window.filter.ignoreAlways["FirefoxCP WebExtensions"] = true

-- ┏━╸┏━┓╻╺┳┓
-- ┃╺┓┣┳┛┃ ┃┃
-- ┗━┛╹┗╸╹╺┻┛
-- Grid

hs.grid.setGrid("3x3", nil, nil)
hs.grid.setMargins({0, 0})
-- For the full list of keycodes look at:
    -- https://github.com/Hammerspoon/hammerspoon/blob/master/extensions/keycodes/keycodes.lua
    -- https://github.com/Hammerspoon/hammerspoon/blob/master/extensions/utf8/utf8.lua#L247-L294
hs.grid.HINTS={
    {'f7', 'f8', 'f9'},
    {'7' , '8' , '9' },
    {'U' , 'I' , 'O' },
    {'J' , 'K' , 'L' },
    {'M' , ',' , '.' }
}

-- UI options: https://github.com/Hammerspoon/hammerspoon/blob/master/extensions/grid/grid.lua#L650
hs.grid.ui.textSize = 200
hs.grid.ui.fontName = 'PT Sans Caption'

-- ┏━┓┏━╸┏━┓╻╺━┓┏━╸   ┏┓     ┏┳┓┏━┓╻ ╻┏━╸
-- ┣┳┛┣╸ ┗━┓┃┏━┛┣╸    ┃╺╋╸   ┃┃┃┃ ┃┃┏┛┣╸
-- ╹┗╸┗━╸┗━┛╹┗━╸┗━╸   ┗━┛    ╹ ╹┗━┛┗┛ ┗━╸
-- Resize and move

local wasPressed = {false, false, false, false}
local pressed = {false, false, false, false}

local function getNextScreen(s)
    all = hs.screen.allScreens()
    for i = 1, #all do
        if all[i] == s then
            return all[(i - 1 + 1) % #all + 1]
        end
    end
    return nil
end

function window.moveToNextScreen()
    local win = hs.window.focusedWindow()
    if win ~= nil then
        currentScreen = win:screen()
        nextScreen = getNextScreen(currentScreen)
        if nextScreen then
            win:moveToScreen(nextScreen)
        end
    end
end

function window.toggleMaximize()
    local win = hs.window.focusedWindow()
    if win ~= nil then
        if isMaximized then
            if previousFrame then
                win:setFrame(previousFrame)
            end
            isMaximized = false
        else
            previousFrame = win:frame()
            win:maximize()
            isMaximized = true
        end
    end
end

local toggleStates = {
    {0, 0, 1/2, 1},
    {0, 0, 1/3, 1},
    {0, 0, 2/3, 1}
}

local rightToggleStates = {
    {1/2, 0, 1/2, 1},
    {2/3, 0, 1/3, 1},
    {1/3, 0, 2/3, 1}
}

local toggleIndex = 1
local rightToggleIndex = 1

local function isWindowInStates(win, states)
    local winFrame = win:frame()
    for _, state in ipairs(states) do
        local stateFrame = hs.geometry.rect(state):fromUnitRect(win:screen():frame())
        if winFrame:equals(stateFrame) then
            return true
        end
    end
    return false
end

function window.toggleLeftSize()
    local win = hs.window.focusedWindow()
    if win ~= nil then
        if not isWindowInStates(win, toggleStates) then
            toggleIndex = 1
        end
        win:move(toggleStates[toggleIndex])
        toggleIndex = (toggleIndex % #toggleStates) + 1
    end
end

function window.toggleRightSize()
    local win = hs.window.focusedWindow()
    if win ~= nil then
        if not isWindowInStates(win, rightToggleStates) then
            rightToggleIndex = 1
        end
        win:move(rightToggleStates[rightToggleIndex])
        rightToggleIndex = (rightToggleIndex % #rightToggleStates) + 1
    end
end

return window
