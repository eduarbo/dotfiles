--                          ░█▀█░█▀▄░█▀▀░█▀▀░▀█▀░█░█
--                          ░█▀▀░█▀▄░█▀▀░█▀▀░░█░░▄▀▄
--                          ░▀░░░▀░▀░▀▀▀░▀░░░▀▀▀░▀░▀

--                 Tmux style hotkey binding: prefix + hotkey

local Prefix = {}
local TIMEOUT = 5

function Prefix:new()
    if Prefix.instance then
        return Prefix
    end

    -- TODO Replace prefix with direct binds to super+hjkl
    local modal = hs.hotkey.modal.new({ "cmd", "ctrl", "alt" }, ",")
    setmetatable({__index = Prefix}, Prefix)

    Prefix.instance = modal

    function modal:entered()
        modal.alertId = hs.alert.show("Prefix Mode", 9999)
        modal.timer =
            hs.timer.doAfter(
            TIMEOUT,
            function()
                modal:exit()
            end
        )
    end

    function modal:exited()
        if modal.alertId then
            hs.alert.closeSpecific(modal.alertId)
        end

        Prefix.cancelTimeout()
    end

    Prefix.bind("", "escape", Prefix.exit)
    Prefix.bind("ctrl", "space", Prefix.exit)

    return Prefix
end

function Prefix.bind(mod, key, fn)
    Prefix.instance:bind(
        mod,
        key,
        nil,
        function()
            fn()
            Prefix.exit()
        end
    )
end

function Prefix.bindMultiple(mod, key, pressedFn, releasedFn, repeatFn)
    Prefix.instance:bind(mod, key, pressedFn, releasedFn, repeatFn)
end

function Prefix.exit()
    Prefix.instance:exit()
end

function Prefix.cancelTimeout()
    if Prefix.instance.timer then
        Prefix.instance.timer:stop()
    end
end

return Prefix:new()
