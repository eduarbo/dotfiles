-----------------------------------------------
-- Press Cmd+Q twice to quit
-----------------------------------------------

local quitModal = hs.hotkey.modal.new("cmd", "q")

local function quitModalExit()
  quitModal:exit()
end

function quitModal:entered()
    hs.alert.closeAll()
    hs.alert.show("Press Cmd+Q again to quit", 1)
    hs.timer.doAfter(1, quitModalExit)
end

local function doQuit()
    hs.application.frontmostApplication():kill()
    quitModal:exit()
end

quitModal:bind("cmd", "q", doQuit)
quitModal:bind("", "escape", quitModalExit)
