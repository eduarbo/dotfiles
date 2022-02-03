import { remap, command, complexModifications } from '../../lib';
import type { ToKeyCodeTuple, KeyCode } from '../../lib';

const remapToLayer = (fromKeyCode: KeyCode, toTuples: ToKeyCodeTuple[]) =>
  remap([fromKeyCode, ['right_option'], ['caps_lock']], toTuples);
const launch = (keyCode: KeyCode, shellCommand: string) =>
  command([keyCode, ['right_option'], ['caps_lock']], shellCommand);

// -- Keys used in other apps
// -- hjkl: arrow keys
// -- o: Alfred
// -- u: launches 1Password mini
// -- y: Clipboard history with Alfred
// -- x: Emojis

const manipulators = [
  launch('a', 'open -a "/System/Applications/Calendar.app"'),
  // remapToLayer('b', [['b', ['option']]]),
  // remapToLayer('c', [['c', ['option']]]),
  launch('d', 'open -a "/Applications/Deepl.app"'),
  launch('e', 'open -a "/Applications/Emacs.app"'),
  launch('f', 'open -a "/System/Library/CoreServices/Finder.app"'),
  launch('g', 'open -a "/Applications/Google Chrome.app"'),
  remapToLayer('h', [['tab', ['left_shift', 'left_command']]]),
  launch('i', 'open -a "/Applications/kitty.app"'),
  remapToLayer('j', [['grave_accent_and_tilde', ['left_command']]]),
  remapToLayer('k', [['grave_accent_and_tilde', ['left_shift', 'left_command']]]),
  remapToLayer('l', [['tab', ['left_command']]]),
  launch('m', 'open -a "/System/Applications/Mail.app"'),
  // remapToLayer('n', [['n', ['option']]]),
  // remapToLayer('o', [['o', ['option']]]),
  launch('p', 'open -a "/Applications/Spotify.app"'),
  launch('q', 'open -a "/Applications/1Password 7.app"'),
  launch('r', 'open -a "/System/Applications/Utilities/Telegram.app"'),
  launch('s', 'open -a "/Applications/Slack.app"'),
  // remapToLayer('t', [['t', ['option']]]),
  // remapToLayer('u', [['u', ['option']]]),
  launch('v', 'open -a "/Applications/Visual Studio Code.app"'),
  launch('w', 'open -a "/Applications/WhatsApp.app"'),
  // remapToLayer('x', [['x', ['option']]]),
  // remapToLayer('y', [['y', ['option']]]),
  launch('z', 'open -a "/Applications/zoom.us.app"'),
  remapToLayer('tab', [['tab', ['option']]]),
  remapToLayer('comma', [['comma', ['option']]]),
  remapToLayer('period', [['period', ['option']]]),
  remapToLayer('slash', [['slash', ['option']]]),
  remapToLayer('backslash', [['backslash', ['option']]]),
];

export const launcher = complexModifications('Launcher', [
  {
    description: 'Launch apps by right option+letters',
    manipulators,
  },
]);
