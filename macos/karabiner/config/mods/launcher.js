// -- Keys used in other apps
// -- hjkl: arrow keys
// -- o: Alfred
// -- u: launches 1Password mini
// -- y: Clipboard history with Alfred
// -- x: Emojis

const appsMap = {
  a: '/System/Applications/Calendar.app',
  // b: '',
  c: '/System/Applications/Utilities/Digital Color Meter.app',
  d: '/Applications/Deepl.app',
  e: '/Applications/Emacs.app',
  f: '/System/Library/CoreServices/Finder.app',
  g: '/Applications/Google Chrome.app',
  i: '/Applications/kitty.app',
  m: '/System/Applications/Mail.app',
  // n: '',
  p: '/Applications/Spotify.app',
  q: '/System/Applications/Utilities/Activity Monitor.app',
  r: '/System/Applications/Utilities/Telegram.app',
  s: '/Applications/Slack.app',
  // t: '',
  // v: '',
  w: '/Applications/WhatsApp.app',
  z: '/Applications/1Password 7.app',
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
  to: [{ shell_command: `open -a "${app}"` }],
}));

module.exports = {
  title: 'Launcher',
  rules: [{
    description: 'Launch apps by right option+letters',
    manipulators,
  }],
};
