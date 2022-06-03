import { remap } from '../../lib';
import type { KeyCode, Modifier, ToKeyCodeTuple, ComplexModifications } from '../../lib';

const mandatoryMods: Modifier[] = ['control', 'option', 'command'];

const keybind = (keyCode: KeyCode, toTuples: ToKeyCodeTuple[], optionalMods: Modifier[] = []) =>
  remap([keyCode, mandatoryMods, optionalMods], toTuples);

const rules = [
  {
    description: 'Meh keys for quick access to the bindings I use most often',
    manipulators: [
      // Go Back/Forward
      keybind('q', [['open_bracket', ['command']]]),
      keybind('t', [['close_bracket', ['command']]]),

      // Previous/Next Tab
      keybind('a', [['open_bracket', ['command', 'shift']]]),
      keybind('g', [['close_bracket', ['command', 'shift']]]),

      // Previous/Next Tab
      keybind('s', [['tab', ['command', 'shift']]]),
      keybind('f', [['tab', ['command']]]),
      keybind('d', [['grave_accent_and_tilde', ['command']]]),
      keybind('e', [['grave_accent_and_tilde', ['command', 'shift']]]),

      // Toggle audio output
      keybind('z', [['z', ['command']]]),
      keybind('x', [['x', ['command']]]),
      keybind('c', [['c', ['command']]]),
      keybind('v', [['v', ['command']]]),
      keybind('b', [['b', ['command', 'shift']]]),
    ],
  },
];

export const meh: ComplexModifications = { title: 'Meh layer', rules };
