local mods = require("modifiers")

-- https://www.reddit.com/r/hammerspoon/comments/t2s6cn/awesomekeys_a_new_way_to_hack_your_key_shortcuts/

hs.loadSpoon("AwesomeKeys")

-- spoon.SpoonInstall.repos.AwesomeKeys = {
--     url = "https://github.com/mobily/awesome-keys",
--     desc = "AwesomeKeys spoon repository",
--     branch = "master"
-- }

local keys = spoon.AwesomeKeys

keys:setGlobalBindings(
    {
        key = "f7",
        fn = function()
            local result =
                hs.dialog.blockAlert(
                "Hammerspoon",
                "You're about to reload hammerspoon config, are you sure?",
                "OK",
                "Cancel"
            )

            if result == "OK" then
                hs.reload()
            end
        end
    }
)

local hyper =
    keys:createHyperBindings(
    {
        -- hyperKey = "/",
        -- hyperMods = mods.meh,
        hyperKey = "/",
        hyperMods = { "ctrl", "alt" },
        backgroundColor = {hex = "#000", alpha = 0.9},
        textColor = {hex = "#FFF", alpha = 0.8},
        modsColor = {hex = "#FA58B6"},
        keyColor = {hex = "#f5d76b"},
        fontFamily = "JetBrains Mono",
        separator = "(✌ ﾟ ∀ ﾟ)☞ –––",
        position = {x = "center", y = "bottom"}
    }
)

-- spoon.SpoonInstall:andUse(
--     "AwesomeKeys",
--     {
--         repo = "AwesomeKeys",
--         fn = startAwesomeKeys
--     }
-- )

-- hyper:setGlobalBindings(
--     {
--         key = "f1",
--         label = "VS Code",
--         fn = keys.fnutils.focusApp("Code")
--     },
--     {
--         key = "f2",
--         label = "iTerm2",
--         fn = keys.fnutils.focusApp("iTerm")
--     },
--     {
--         key = "f3",
--         label = "Chrome",
--         fn = keys.fnutils.focusApp("Google Chrome")
--     },
--     {
--         key = "f4",
--         label = "Slack",
--         fn = keys.fnutils.focusApp("Slack")
--     },
--     {
--         mods = {"command"},
--         key = "-",
--         label = "Zoom",
--         fn = keys.fnutils.focusApp("zoom.us")
--     },
--     {
--         mods = {"command"},
--         key = "=",
--         label = "Obsidian",
--         fn = keys.fnutils.focusApp("Obsidian")
--     },
--     {
--         mods = {"control"},
--         key = "\\",
--         label = "Sleep",
--         fn = function()
--             hs.caffeinate.systemSleep()
--         end
--     },
--     {
--         mods = {"control"},
--         key = "]",
--         label = "Lock",
--         fn = function()
--             hs.caffeinate.lockScreen()
--         end
--     }
-- )
