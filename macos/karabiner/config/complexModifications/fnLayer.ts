import { remap, modTap } from '../../lib';
import type { KeyCode, Modifier, ToKeyCodeTuple, ComplexModifications } from '../../lib';

const LAYER = 'FN';
const layerMods: Modifier[] = ['shift', 'option', 'command', 'control'];
const optionalMods: Modifier[] = ['shift'];

const keybind = (keyCode: KeyCode, toTuples: ToKeyCodeTuple[]) =>
  remap([keyCode, layerMods, optionalMods], toTuples);

const rules = [
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      modTap(
        ['left_command', ['option', 'command', 'control'], optionalMods],
        [['left_shift', ['option', 'command', 'control']]],
        [['spacebar']],
      ),
      modTap(
        ['right_command', ['option', 'command', 'control'], optionalMods],
        [['right_shift', ['option', 'command', 'control']]],
        [['escape']],
      ),
    ],
  },
  {
    description: `${LAYER} layer: Right hand - dev tools, screenshots and window manager`,
    manipulators: [
      /// Top Row - Dev Tools
      keybind('q', [['j', ['option', 'command']]]),
      keybind('w', [['m', ['shift', 'command']]]),
      keybind('e', [['i', ['option', 'command']]]),
      keybind('r', [['c', ['shift', 'command']]]),
      keybind('t', [['backslash', ['command']]]),

      /// Home Row - Reserved to manage windows with HammerSpoon 🔨🥄
      keybind('a', [['a', layerMods]]),
      keybind('s', [['s', layerMods]]),
      keybind('d', [['d', layerMods]]),
      keybind('f', [['f', layerMods]]),
      keybind('g', [['g', layerMods]]),

      /// Bottom Row - Screenshots
      // Z key: available
      keybind('z', [['z', layerMods]]),
      keybind('x', [['3', ['shift', 'command']]]),
      keybind('c', [['4', ['shift', 'command']]]),
      keybind('v', [['5', ['shift', 'command']]]),
      // B key: reserved to move window to next screen with HammerSpoon 🔨🥄
      keybind('b', [['b', layerMods]]),
    ],
  },
  {
    description: `${LAYER} layer: Left hand - media controls`,
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
