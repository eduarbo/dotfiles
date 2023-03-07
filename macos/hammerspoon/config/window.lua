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

hs.grid.setGrid("5x3", nil, nil)
hs.grid.setMargins({0, 0})
-- hs.grid.HINTS={
--     {'f1','f2','f3','f4','f5'},
--     {'1','2','3','4','5'},
--     {'Q','W','E','R','T'},
--     {'A','S','D','F','G'},
--     {'Z','X','C','V','B'}
-- }

-- ┏━┓┏━╸┏━┓╻╺━┓┏━╸   ┏┓     ┏┳┓┏━┓╻ ╻┏━╸
-- ┣┳┛┣╸ ┗━┓┃┏━┛┣╸    ┃╺╋╸   ┃┃┃┃ ┃┃┏┛┣╸
-- ╹┗╸┗━╸┗━┛╹┗━╸┗━╸   ┗━┛    ╹ ╹┗━┛┗┛ ┗━╸
-- Resize and move

local wasPressed = {false, false, false, false}
local pressed = {false, false, false, false}

local function resizeWindow(arrowKeys)
    local leftHalf = arrowKeys[1]
    local leftThird = arrowKeys[2]
    local rightThird = arrowKeys[3]
    local rightHalf = arrowKeys[4]

    local rectMap = {
        [leftHalf] = {0, 0, 0.5, 1}, -- left half
        [rightHalf] = {0.5, 0, 0.5, 1}, -- right half
        [leftThird] = {0, 0, 1 / 3, 1}, -- left one third
        [rightThird] = {2 / 3, 0, 1 / 3, 1}, -- right one third
        [leftHalf .. rightThird] = {0, 0, 2 / 3, 1}, -- left two thirds
        [rightHalf .. leftThird] = {1 / 3, 0, 2 / 3, 1}, -- right two thirds
        [leftHalf .. rightHalf] = {0, 0, 1, 1}, -- full screen
        [leftThird .. rightThird] = "center" -- center on screen
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
