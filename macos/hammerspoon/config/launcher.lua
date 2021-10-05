-- Keys used in other apps
-- hjkl: arrow keys
-- o: Alfred
-- t: translates selected text with DeepL
-- u: launches 1Password mini
-- y: Clipboard history with Alfred

apps = {
    a = "Calendar",
    -- b = "",
    c = "Digital Color Meter",
    d = "DeepL",
    e = "Emacs",
    f = "Finder",
    g = "Google Chrome",
    i = "kitty",
    m = "Mail",
    -- n = "",
    p = "Spotify",
    q = "Activity Monitor",
    r = "Telegram",
    s = "Slack",
    -- v = "",
    w = "WhatsApp",
    x = "Firefox Developer Edition",
    z = "1Password 7"
}

for key, app in pairs(apps) do
    hs.hotkey.bind(
        "rightalt",
        key,
        function()
            hs.application.launchOrFocus(app)
        end
    )
end
