--                 ░█░█░▀█▀░█▀█░░░█▄█░█▀█░█▀█░█▀█░█▀▀░█▀▀░█▀▄
--                 ░█▄█░░█░░█░█░░░█░█░█▀█░█░█░█▀█░█░█░█▀▀░█▀▄
--                 ░▀░▀░▀▀▀░▀░▀░░░▀░▀░▀░▀░▀░▀░▀░▀░▀▀▀░▀▀▀░▀░▀

local mods = require("modifiers")

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
hs.hotkey.bind(mods.super, "z", hs.grid.show)

-- ┏━┓┏━╸┏━┓╻╺━┓┏━╸   ┏┓     ┏┳┓┏━┓╻ ╻┏━╸
-- ┣┳┛┣╸ ┗━┓┃┏━┛┣╸    ┃╺╋╸   ┃┃┃┃ ┃┃┏┛┣╸
-- ╹┗╸┗━╸┗━┛╹┗━╸┗━╸   ┗━┛    ╹ ╹┗━┛┗┛ ┗━╸
-- Resize and move

local arrowKeys = {"n", "m", ",", "."}
local rectMap = {
    ["n"] = {0, 0, 0.5, 1}, -- left half
    ["."] = {0.5, 0, 0.5, 1}, -- right half
    ["m"] = {0, 0, 1 / 3, 1}, -- left one third
    [","] = {2 / 3, 0, 1 / 3, 1}, -- right one third
    ["nm"] = {0, 0, 2 / 3, 1}, -- left two thirds
    [",."] = {1 / 3, 0, 2 / 3, 1}, -- right two thirds
    ["n."] = {0, 0, 1, 1}, -- full screen
    ["m,"] = "center" -- center on screen
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
    hs.hotkey.bind(mods.meh, arrowKeys[i], pressedFn, releasedFn, nil)
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

hs.hotkey.bind(mods.super, "b", moveToNextScreen)
