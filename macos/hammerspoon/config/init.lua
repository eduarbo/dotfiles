--                ░█░█░█▀█░█▄█░█▄█░█▀▀░█▀▄░█▀▀░█▀█░█▀█░█▀█░█▀█
--                ░█▀█░█▀█░█░█░█░█░█▀▀░█▀▄░▀▀█░█▀▀░█░█░█░█░█░█
--                ░▀░▀░▀░▀░▀░▀░▀░▀░▀▀▀░▀░▀░▀▀▀░▀░░░▀▀▀░▀▀▀░▀░▀

local log = require("log")

-- adjust hotkey logging... info as the default is too much.
hs.hotkey.setLogLevel("warning")
hs.logger.historySize(1000)
log.i("Initializing")

-- Reload config on write
hs.pathwatcher.new(hs.configdir, hs.reload):start()
hs.notify.new({ title = "Config reloaded 🔨🥄", withdrawAfter = 1 }):send()

-- Alert styling
hs.alert.defaultStyle.strokeColor = {white = 0, alpha = 0}
hs.alert.defaultStyle.fillColor = {white = 0, alpha = 0.75}
hs.alert.defaultStyle.textStyle = {paragraphStyle = {alignment = "center"}}
hs.alert.defaultStyle.textSize = 25

-----------------------------------------------
-- Press Cmd+Q twice to quit
-----------------------------------------------

local quitModal = hs.hotkey.modal.new("cmd", "q")

function quitModal:entered()
    hs.alert.closeAll()
    hs.alert.show("Press Cmd+Q again to quit", 1)
    hs.timer.doAfter(
        1,
        function()
            quitModal:exit()
        end
    )
end

local function doQuit()
    hs.application.frontmostApplication():kill()
    quitModal:exit()
end

quitModal:bind("cmd", "q", doQuit)
quitModal:bind(
    "",
    "escape",
    function()
        quitModal:exit()
    end
)

-----------------------------------------------
-- Lock system
-----------------------------------------------
hs.hotkey.bind({"rightalt"}, "`", hs.toggleConsole)

-----------------------------------------------
-- Modules
-----------------------------------------------

require("prefix")
require("date-battery")
require("mic")
require("netspeed")
require("window")
require("audio")
