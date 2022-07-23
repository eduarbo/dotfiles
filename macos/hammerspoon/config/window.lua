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
hs.grid.HINTS={
    {'f6','f7','f8','f9','f10'},
    {'6','7','8','9','0'},
    {'Y','U','I','O','P'},
    {'H','J','K','L',';'},
    {'N','M',',','.','/'}
}
hs.hotkey.bind(mods.super, "y", hs.grid.show)

-- ┏━┓┏━╸┏━┓╻╺━┓┏━╸   ┏┓     ┏┳┓┏━┓╻ ╻┏━╸
-- ┣┳┛┣╸ ┗━┓┃┏━┛┣╸    ┃╺╋╸   ┃┃┃┃ ┃┃┏┛┣╸
-- ╹┗╸┗━╸┗━┛╹┗━╸┗━╸   ┗━┛    ╹ ╹┗━┛┗┛ ┗━╸
-- Resize and move

local arrowKeys = {"h", "j", "k", "l"}
local rectMap = {
    ["h"] = {0, 0, 0.5, 1}, -- left half
    ["l"] = {0.5, 0, 0.5, 1}, -- right half
    ["j"] = {0, 0, 1 / 3, 1}, -- left one third
    ["k"] = {2 / 3, 0, 1 / 3, 1}, -- right one third
    ["hj"] = {0, 0, 2 / 3, 1}, -- left two thirds
    ["kl"] = {1 / 3, 0, 2 / 3, 1}, -- right two thirds
    ["hl"] = {0, 0, 1, 1}, -- full screen
    ["jk"] = "center" -- center on screen
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
    hs.hotkey.bind(mods.super, arrowKeys[i], pressedFn, releasedFn, nil)
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

hs.hotkey.bind(mods.super, "n", moveToNextScreen)

-- -- super + hjkl -> move window

-- local DX = {-1, 0, 0, 1}
-- local DY = {0, 1, -1, 0}
-- local DELTA = 20

-- for i = 1, 4 do
--     local function moveWin()
--         local win = hs.window.focusedWindow()
--         if win ~= nil then
--             local p = win:topLeft()
--             p.x = p.x + DX[i] * DELTA
--             p.y = p.y + DY[i] * DELTA
--             win:setTopLeft(p)
--         end
--     end
--     local function pressedFn()
--         moveWin()
--     end
--     hs.hotkey.bind(mods.hyper, arrowKeys[i], pressedFn, nil, moveWin)
-- end

-- -- ┏━┓╻ ╻┏━┓╻┏┓╻╻┏     ╻   ┏━╸╻ ╻┏━┓┏━┓┏┓╻╺┳┓   ┏━╸┏━┓┏━┓┏┳┓┏━╸
-- -- ┗━┓┣━┫┣┳┛┃┃┗┫┣┻┓   ┏┛   ┣╸ ┏╋┛┣━┛┣━┫┃┗┫ ┃┃   ┣╸ ┣┳┛┣━┫┃┃┃┣╸
-- -- ┗━┛╹ ╹╹┗╸╹╹ ╹╹ ╹   ╹    ┗━╸╹ ╹╹  ╹ ╹╹ ╹╺┻┛   ╹  ╹┗╸╹ ╹╹ ╹┗━╸
-- -- Shrink/Expand frame

-- -- meh + , -> shrink window frame
-- -- meh + . -> expand window frame

-- local function expandWin(ratio)
--     local win = hs.window.focusedWindow()
--     if win == nil then
--         return
--     end
--     frame = win:frame()
--     local cx = frame.x + frame.w / 2
--     local cy = frame.y + frame.h / 2
--     local nw = frame.w * ratio
--     local nh = frame.h * ratio
--     local nx = cx - nw / 2
--     local ny = cy - nh / 2
--     win:setFrame(hs.geometry.rect(nx, ny, nw, nh))
-- end

-- local function bindExpandWin(ratio)
--     return function()
--         expandWin(ratio)
--     end
-- end

-- local function pressedExpandWin(ratio)
--     return function()
--         expandWin(ratio)
--     end
-- end

-- hs.hotkey.bind(mods.meh, ",", pressedExpandWin(0.9), nil, bindExpandWin(0.9))
-- hs.hotkey.bind(mods.meh, ".", pressedExpandWin(1.1), nil, bindExpandWin(1.1))

-- -- ┏━┓╻ ╻┏━┓╻┏┓╻╻┏     ╻   ┏━╸╻ ╻┏━┓┏━┓┏┓╻╺┳┓   ┏━╸╺┳┓┏━╸┏━╸┏━┓
-- -- ┗━┓┣━┫┣┳┛┃┃┗┫┣┻┓   ┏┛   ┣╸ ┏╋┛┣━┛┣━┫┃┗┫ ┃┃   ┣╸  ┃┃┃╺┓┣╸ ┗━┓
-- -- ┗━┛╹ ╹╹┗╸╹╹ ╹╹ ╹   ╹    ┗━╸╹ ╹╹  ╹ ╹╹ ╹╺┻┛   ┗━╸╺┻┛┗━┛┗━╸┗━┛
-- -- Shrink/Expand Edges

-- -- super + hjkl -> expand window edges
-- -- hyper + hjkl -> shrink window edges

-- local function expandEdge(edge, ratio)
--     local win = hs.window.focusedWindow()
--     if win == nil then
--         return
--     end
--     frame = win:frame()
--     local x, y, w, h = frame.x, frame.y, frame.w, frame.h
--     if edge == "h" then
--         w = frame.w * ratio
--         x = frame.x + frame.w - w
--     elseif edge == "j" then
--         h = frame.h * ratio
--     elseif edge == "k" then
--         h = frame.h * ratio
--         y = frame.y + frame.h - h
--     elseif edge == "l" then
--         w = frame.w * ratio
--     else
--         return
--     end
--     win:setFrame(hs.geometry.rect(x, y, w, h))
-- end

-- local edges = {"h", "j", "k", "l"}
-- local ratios = {0.9, 1.111111}

-- for i = 1, #edges do
--     local edge = edges[i]
--     for j = 1, #ratios do
--         local mod = (ratios[j] > 1) and mods.super or mods.hyper
--         local function fn()
--             expandEdge(edge, ratios[j])
--         end
--         local function pressedFn()
--             fn()
--         end
--         hs.hotkey.bind(mod, edge, pressedFn, nil, fn)
--     end
-- end
