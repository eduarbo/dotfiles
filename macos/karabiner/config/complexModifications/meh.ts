import { remap } from '../../lib';
import type { KeyCode, Modifier, ToKeyCodeTuple, ComplexModifications } from '../../lib';

const mehMods: Modifier[] = ['shift', 'option', 'control'];

const keybind = (keyCode: KeyCode, toTuples: ToKeyCodeTuple[], optionalMods: Modifier[] = ['any']) =>
  remap([keyCode, null, optionalMods], toTuples, {
    manipulatorOptions: {
      conditions: [
        {
          type: 'variable_if',
          name: 'MEH',
          value: true,
        },
      ],
    },
  });

const rules = [
  {
    description: 'Quick access to frequently used bindings in left hand',
    manipulators: [
      /// Top Row - Dev Tools
      keybind('q', [['j', ['right_shift', 'right_command']]]),
      keybind('w', [['m', ['right_shift', 'right_command']]]),
      keybind('e', [['i', ['right_shift', 'right_command']]]),
      keybind('r', [['c', ['right_shift', 'right_command']]]),
      keybind('t', [['backslash', ['right_shift', 'right_command']]]),

      /// Home Row - Reserved to manage windows with HammerSpoon 🔨🥄
      keybind('a', [['a', mehMods]]),
      keybind('s', [['s', mehMods]]),
      keybind('d', [['d', mehMods]]),
      keybind('f', [['f', mehMods]]),
      keybind('g', [['g', mehMods]]),

      /// Bottom Row - Screenshots
      // Z key: available
      keybind('z', [['z', mehMods]]),
      keybind('x', [['3', ['right_shift', 'right_command']]]),
      keybind('c', [['4', ['right_shift', 'right_command']]]),
      keybind('v', [['5', ['right_shift', 'right_command']]]),
      // B key: reserved to move window to next screen with HammerSpoon 🔨🥄
      keybind('b', [['b', mehMods]]),
    ],
  },
  {
    description: 'Function keys in right hand',
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

      keybind('right_command', [['right_shift']]),
    ],
  },
];

export const meh: ComplexModifications = { title: 'Meh layer', rules };
