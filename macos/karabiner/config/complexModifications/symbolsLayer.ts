import { remap, ignoreKeebs } from '../../lib/index.js';
import type {
  Modifier,
  ModifierOptional,
  ToKeyCodeTuple,
  KeyCode,
  ComplexModifications,
} from '../../lib/index.js';

const LAYER = 'SYMBOLS';
const layerMods: Modifier[] = ['right_shift'];
const shiftedLayerMods: Modifier[] = ['left_shift', 'right_shift'];
const optionalMods: ModifierOptional[] = [
  'left_shift',
  'right_command',
  'right_control',
  'caps_lock',
];

const manipulatorOptions = {
  conditions: ignoreKeebs,
};

const keybind = (
  fromKeyCode: KeyCode,
  toTuples: ToKeyCodeTuple[],
  options?: { shifted: boolean },
) =>
  remap([fromKeyCode, options?.shifted ? shiftedLayerMods : layerMods, optionalMods], toTuples, {
    manipulatorOptions,
  });

const rules = [
  {
    description: `${LAYER} layer: Left hand - Numpad`,
    manipulators: [
      keybind('q', [['page_up']]),
      keybind('q', [['home']], { shifted: true }),
      keybind('w', [['7']]),
      keybind('e', [['8']]),
      keybind('r', [['9']]),
      keybind('t', [['0']]),

      keybind('a', [['e', ['option']]]),
      keybind('s', [['4']]),
      keybind('d', [['5']]),
      keybind('f', [['6']]),
      keybind('g', [['equal_sign']]),

      keybind('z', [['caps_lock']]),
      keybind('x', [['1']]),
      keybind('c', [['2']]),
      keybind('v', [['3']]),
      keybind('b', [['hyphen']]),
    ],
  },
  {
    description: `${LAYER} layer: Right hand - Symbols and arrows`,
    manipulators: [
      keybind('y', [['backslash']]),
      keybind('u', [['open_bracket']]),
      keybind('i', [['close_bracket']]),
      keybind('o', [['slash']]),
      keybind('p', [['page_down']]),
      keybind('p', [['end']], { shifted: true }),

      keybind('h', [['left_arrow']]),
      keybind('j', [['down_arrow']]),
      keybind('k', [['up_arrow']]),
      keybind('l', [['right_arrow']]),
      keybind('semicolon', [['n', ['option']]]),

      keybind('n', [['grave_accent_and_tilde']]),
      keybind('m', [['quote']]),
      keybind('comma', [['comma']]),
      keybind('period', [['period']]),
      keybind('slash', [['delete_or_backspace']]),
    ],
  },
];

export const symbolsLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
