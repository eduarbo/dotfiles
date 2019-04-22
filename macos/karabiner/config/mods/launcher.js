const appsMap = {
  a: '/Applications/Calendar.app',
  c: '/Applications/Utilities/Digital Color Meter.app',
  b: '/Applications/Karabiner-Elements.app',
  d: '/Applications/1Password 7.app',
  e: '/Applications/Emacs.app',
  f: '/System/Library/CoreServices/Finder.app',
  g: '/Applications/Google Chrome.app',
  i: '/Applications/kitty.app',
  m: '/Applications/Mail.app',
  n: '/Applications/Notes.app',
  p: '/Applications/Spotify.app',
  r: '/Applications/Utilities/Activity Monitor.app',
  s: '/Applications/Slack.app',
  t: '/Applications/iTerm.app',
  q: '/System/Library/Frameworks/ScreenSaver.framework/Versions/A/Resources/ScreenSaverEngine.app',
};

const manipulators = Object.entries(appsMap).map(([key, app]) => ({
  type: 'basic',
  from: {
    key_code: key,
    modifiers: {
      mandatory: ['right_option'],
      optional: ['caps_lock'],
    },
  },
  to: [{ shell_command: `open '${app}'` }],
}));

module.exports = {
  title: 'Launcher (@eduarbo)',
  rules: [{
    description: 'Launch apps by right option+letters.',
    manipulators,
  }],
};
