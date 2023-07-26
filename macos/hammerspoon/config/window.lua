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

local function resizeWindow(arrowKeys)
    local leftHalf = arrowKeys[1]
    local rightHalf = arrowKeys[2]

    local rectMap = {
        [leftHalf] = {0, 0, 0.5, 1},
        [rightHalf] = {0.5, 0, 0.5, 1},
        [leftHalf .. rightHalf] = {0, 0, 1, 1} -- full screen
    }

    for i = 1, #pressed do
        if pressed[i] then
            return
        end
    end

    local win = hs.window.focusedWindow()
    if win ~= nil then
        local keys = ""
        for i = 1, #wasPressed do
            if wasPressed[i] then
                keys = keys .. arrowKeys[i]
                wasPressed[i] = false
            end
        end
        local rect = rectMap[keys]
        if rect ~= nil then
            if rect == "center" then
                win:centerOnScreen()
            else
                win:move(rect)
            end
        end
    end
end

function window.bindResize(layerMods, arrowKeys)
    for i = 1, #arrowKeys do
        local function pressedFn()
            wasPressed[i] = true
            pressed[i] = true
        end
        local function releasedFn()
            pressed[i] = false
            resizeWindow(arrowKeys)
        end
        hs.hotkey.bind(layerMods, arrowKeys[i], pressedFn, releasedFn, nil)
    end
end

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

return window
