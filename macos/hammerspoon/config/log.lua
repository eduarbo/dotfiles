local log = hs.logger.new("init", "debug")

-- adjust hotkey logging... info as the default is too much.
hs.hotkey.setLogLevel("warning")
hs.logger.historySize(1000)

return log
