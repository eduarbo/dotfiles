local mods = require("modifiers")

spoon.SpoonInstall:andUse("KSheet", {
  hotkeys = {
    toggle = { mods.meh, "/" }
  }
})
