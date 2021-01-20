const appsMap = {
  a: '/System/Applications/Calendar.app',
  c: '/System/Applications/Utilities/Digital Color Meter.app',
  d: '/Applications/1Password 7.app',
  e: '/Applications/Emacs.app',
  f: '/System/Library/CoreServices/Finder.app',
  x: '/Applications/Firefox Developer Edition.app',
  g: '/Applications/Google Chrome.app',
  i: '/Applications/kitty.app',
  m: '/System/Applications/Mail.app',
  p: '/Applications/Spotify.app',
  r: '/System/Applications/Utilities/Activity Monitor.app',
  s: '/Applications/Slack.app',
  v: '/Applications/Karabiner-EventViewer.app',
  w: '/Applications/WhatsApp.app',
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
  title: 'Launcher',
  rules: [{
    description: 'Launch apps by right option+letters',
    manipulators,
  }],
};
