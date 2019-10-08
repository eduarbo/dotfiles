-- ░█░█░█▀█░█▀▀░▀█▀░█▀▀░░░█▀▀░█░█░▀█▀░▀█▀░█▀▀░█░█░█▀▀░█▀▄
-- ░█▀█░█░█░▀▀█░░█░░▀▀█░░░▀▀█░█▄█░░█░░░█░░█░░░█▀█░█▀▀░█▀▄
-- ░▀░▀░▀▀▀░▀▀▀░░▀░░▀▀▀░░░▀▀▀░▀░▀░▀▀▀░░▀░░▀▀▀░▀░▀░▀▀▀░▀░▀


local hostsSwitcherPath = os.getenv("HOME") .. '/.config/hosts'
local hostsSelectedSymlink = hostsSwitcherPath .. '/.selected'


local function updateHostsSwitcherTitle()
  local hostsSelectedRealPath = hs.fs.pathToAbsolute(hostsSelectedSymlink)

  if hostsSelectedRealPath then
    local hostsSelected = hostsSelectedRealPath:match("^.+/(.+)$")
    hostsSwitcherMenubar:setTitle(hostsSelected)
  end
end

-- Check if selected hosts file changed and update the menu title
local function onHostsChange(paths)
  for i = 1, #paths, 1 do
    if paths[i]:match("%.selected$") then
      updateHostsSwitcherTitle()
      break
    end
  end
end

local function initHostsSwitcher()
  hostsSwitcherMenubar = hs.menubar.new()
  hostsSwitcherMenubar:setIcon('./icons/dns.pdf', true)
  updateHostsSwitcherTitle()

  hs.pathwatcher.new(hostsSwitcherPath, onHostsChange):start()

  -- TODO Select hosts file from a menu list
  -- hostsSwitcherMenubar:setMenu({
  --     { title = "my menu item", fn = function() print("you clicked my menu item!") end },
  --     -- { title = "-" },
  --     { title = "other item", fn = some_function },
  --     { title = "disabled item", disabled = true },
  --     { title = "checked item", checked = true },
  -- })
end

if not hostsSwitcherMenubar then
  initHostsSwitcher()
end
