--                 ░█░█░▀█▀░█▀█░░░█▄█░█▀█░█▀█░█▀█░█▀▀░█▀▀░█▀▄
--                 ░█▄█░░█░░█░█░░░█░█░█▀█░█░█░█▀█░█░█░█▀▀░█▀▄
--                 ░▀░▀░▀▀▀░▀░▀░░░▀░▀░▀░▀░▀░▀░▀░▀░▀▀▀░▀▀▀░▀░▀

local mods = require("modifiers")
local layerMods = mods.hyper

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
hs.grid.HINTS={
    {'f6','f7','f8','f9','f10'},
    {'6','7','8','9','0'},
    {'Y','U','I','O','P'},
    {'H','J','K','L',';'},
    {'N','M',',','.','/'}
}
hs.hotkey.bind(layerMods, "a", hs.grid.show)

-- ┏━┓┏━╸┏━┓╻╺━┓┏━╸   ┏┓     ┏┳┓┏━┓╻ ╻┏━╸
-- ┣┳┛┣╸ ┗━┓┃┏━┛┣╸    ┃╺╋╸   ┃┃┃┃ ┃┃┏┛┣╸
-- ╹┗╸┗━╸┗━┛╹┗━╸┗━╸   ┗━┛    ╹ ╹┗━┛┗┛ ┗━╸
-- Resize and move

local arrowKeys = {"s", "d", "f", "g"}
local rectMap = {
    ["s"] = {0, 0, 0.5, 1}, -- left half
    ["g"] = {0.5, 0, 0.5, 1}, -- right half
    ["d"] = {0, 0, 1 / 3, 1}, -- left one third
    ["f"] = {2 / 3, 0, 1 / 3, 1}, -- right one third
    ["sd"] = {0, 0, 2 / 3, 1}, -- left two thirds
    ["fg"] = {1 / 3, 0, 2 / 3, 1}, -- right two thirds
    ["sg"] = {0, 0, 1, 1}, -- full screen
    ["df"] = "center" -- center on screen
}
local wasPressed = {false, false, false, false}
local pressed = {false, false, false, false}

local function resizeWindow()
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

for i = 1, #arrowKeys do
    local function pressedFn()
        wasPressed[i] = true
        pressed[i] = true
    end
    local function releasedFn()
        pressed[i] = false
        resizeWindow()
    end
    hs.hotkey.bind(layerMods, arrowKeys[i], pressedFn, releasedFn, nil)
end

-- meh + n -> move window to the next screen

local function getNextScreen(s)
    all = hs.screen.allScreens()
    for i = 1, #all do
        if all[i] == s then
            return all[(i - 1 + 1) % #all + 1]
        end
    end
    return nil
end

local function moveToNextScreen()
    local win = hs.window.focusedWindow()
    if win ~= nil then
        currentScreen = win:screen()
        nextScreen = getNextScreen(currentScreen)
        if nextScreen then
            win:moveToScreen(nextScreen)
        end
    end
end

hs.hotkey.bind(layerMods, "b", moveToNextScreen)
