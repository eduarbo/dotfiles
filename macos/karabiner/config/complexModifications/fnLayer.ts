import { remap, modTap } from '../../lib';
import type { KeyCode, Modifier, ToKeyCodeTuple, ComplexModifications } from '../../lib';

const LAYER = 'FN';
const layerMods: Modifier[] = ['option', 'command', 'control'];
const optionalMods: Modifier[] = [];

const keybind = (keyCode: KeyCode, toTuples: ToKeyCodeTuple[]) =>
  remap([keyCode, layerMods, optionalMods], toTuples);

const rules = [
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      // L COMMAND: SPACE on tap | HYPER on hold
      modTap(
        ['left_command', layerMods, optionalMods],
        [['left_shift', layerMods]],
        [['spacebar']],
      ),
      // R COMMAND: ESC on tap | HYPER on hold
      modTap(
        ['right_command', layerMods, optionalMods],
        [['left_shift', layerMods]],
        [['escape']],
      ),
    ],
  },
  {
    description: `${LAYER} layer: Left hand - dev tools, screenshots and window manager`,
    manipulators: [
      /// Top Row - Dev Tools
      // Toggle JS Console
      keybind('q', [['j', ['option', 'command']]]),
      // Toggle breakpoints
      keybind('w', [['f8', ['shift', 'command']]]),
      // Toggle DevTools
      keybind('e', [['i', ['option', 'command']]]),
      // Inspect element
      keybind('r', [['c', ['shift', 'command']]]),
      // Toggle script execution
      keybind('t', [['backslash', ['command']]]),

      /// Page Nav
      // A key: available
      keybind('a', [['a', layerMods]]),
      keybind('s', [['home']]),
      keybind('d', [['page_down']]),
      keybind('f', [['page_up']]),
      keybind('g', [['end']]),

      /// Bottom Row - Screenshots
      // Z key: reserved to resize window with HammerSpoon 🔨🥄
      keybind('z', [['z', layerMods]]),
      // screenshot
      keybind('x', [['3', ['shift', 'command']]]),
      // capture a portion of the screen
      keybind('c', [['4', ['shift', 'command']]]),
      // record screen
      keybind('v', [['5', ['shift', 'command']]]),
      // B key: reserved to move window to next screen with HammerSpoon 🔨🥄
      keybind('b', [['b', layerMods]]),
    ],
  },
  {
    description: `${LAYER} layer: Right hand - media controls`,
    manipulators: [
      /// Top Row
      keybind('y', [['volume_increment']]),
      // keybind('y', [['f12']]),
      keybind('u', [['rewind']]),
      // keybind('u', [['f7']]),
      keybind('i', [['play_or_pause']]),
      // keybind('i', [['f8']]),
      keybind('o', [['fastforward']]),
      // keybind('o', [['f9']]),
      keybind('p', [['print_screen']]),

      /// Home Row
      keybind('h', [['volume_decrement']]),
      // keybind('h', [['f11']]),
      keybind('j', [['f4']]),
      keybind('k', [['f5']]),
      keybind('l', [['f6']]),
      keybind('semicolon', [['scroll_lock']]),

      /// Bottom Row
      keybind('n', [['mute']]),
      // keybind('n', [['f10']]),
      keybind('m', [['display_brightness_decrement']]),
      // keybind('m', [['f1']]),
      keybind('comma', [['display_brightness_increment']]),
      // keybind('comma', [['f2']]),
      keybind('period', [['mission_control']]),
      // keybind('period', [['f3']]),
      keybind('slash', [['pause']]),
    ],
  },
];

export const fnLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
