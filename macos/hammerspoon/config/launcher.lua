apps = {
    a = "Calendar",
    c = "Digital Color Meter",
    d = "DeepL",
    e = "Emacs",
    f = "Finder",
    g = "Google Chrome",
    i = "kitty",
    m = "Mail",
    p = "Spotify",
    q = "Activity Monitor",
    s = "Slack",
    t = "Telegram",
    u = "1Password 7",
    w = "WhatsApp",
    x = "Firefox Developer Edition"
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
