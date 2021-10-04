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
    u = "1Password 7",
    s = "Slack",
    t = "Telegram",
    w = "WhatsApp",
    x = "Firefox Developer Edition"
}

for key, app in pairs(apps) do
    hs.hotkey.bind(
        "alt",
        key,
        function()
            hs.application.launchOrFocus(app)
        end
    )
end
