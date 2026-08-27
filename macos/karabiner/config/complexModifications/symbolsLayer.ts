import { modTap, remap, ignoreKeebs } from '../../lib/index.js';
import type {
  Modifier,
  ModifierOptional,
  ToKeyCodeTuple,
  KeyCode,
  ComplexModifications,
} from '../../lib/index.js';
import { SUPER_LAYER_VARIABLE, SYMBOLS_LAYER_VARIABLE } from './layerVariables.js';

const LAYER = 'SYMBOLS';
const shiftedLayerMods: Modifier[] = ['shift'];
const optionalMods: ModifierOptional[] = ['shift', 'right_command', 'right_control', 'caps_lock'];
const shiftedOptionalMods: ModifierOptional[] = [
  'right_command',
  'right_control',
  'caps_lock',
];

const manipulatorOptions = {
  conditions: [
    ...ignoreKeebs,
    { type: 'variable_if' as const, name: SYMBOLS_LAYER_VARIABLE, value: 1 },
  ],
};

const keybind = (
  fromKeyCode: KeyCode,
  toTuples: ToKeyCodeTuple[],
  options?: { shifted: boolean },
) =>
  remap(
    [
      fromKeyCode,
      options?.shifted ? shiftedLayerMods : null,
      options?.shifted ? shiftedOptionalMods : optionalMods,
    ],
    toTuples,
    { manipulatorOptions },
  );

const rules = [
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      // Keep Left Shift lazy on hold; use the opposite Shift side for the tap chord.
      modTap(
        ['left_command', null, optionalMods],
        [['left_shift']],
        [['spacebar', ['right_shift']]],
        { manipulatorOptions, toOptions: { lazy: true } },
      ),
      // Command and Return share a mod-tap key, so expose their chord through SYMBOLS+Shift.
      keybind('spacebar', [['return_or_enter', ['left_command']]], { shifted: true }),
      // Keep Right Command lazy on hold; use Left Command for the tap chord.
      modTap(
        ['spacebar', null, optionalMods],
        [['right_command']],
        [['return_or_enter', ['left_command']]],
        { manipulatorOptions, toOptions: { lazy: true } },
      ),
      // Preserve the launcher chord that previously inherited Shift from SYMBOLS.
      modTap(
        ['left_option', null, optionalMods],
        [['left_option', ['left_command', 'left_control']]],
        [['escape', ['right_shift']]],
        {
          manipulatorOptions,
          toOptions: { lazy: true },
          setVariables: {
            [SUPER_LAYER_VARIABLE]: { to: 1, to_after_key_up: 0 },
          },
        },
      ),
    ],
  },
  {
    description: `${LAYER} layer: Left hand - Numpad`,
    manipulators: [
      // Shift-specific bindings must precede the generic layer bindings.
      keybind('q', [['home']], { shifted: true }),
      keybind('q', [['page_up']]),
      keybind('w', [['7']]),
      keybind('e', [['8']]),
      keybind('r', [['9']]),
      keybind('t', [['0']]),

      // Tap for acute accent; hold to add Shift within SYMBOLS.
      modTap(['a', null, optionalMods], [['left_shift']], [['e', ['option']]], {
        manipulatorOptions,
      }),
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
      keybind('p', [['end']], { shifted: true }),
      keybind('p', [['page_down']]),

      keybind('h', [['left_arrow']]),
      keybind('j', [['down_arrow']]),
      keybind('k', [['up_arrow']]),
      keybind('l', [['right_arrow']]),
      // Tap for tilde; hold to add Shift within SYMBOLS.
      modTap(['semicolon', null, optionalMods], [['left_shift']], [['n', ['option']]], {
        manipulatorOptions,
      }),

      keybind('n', [['grave_accent_and_tilde']]),
      keybind('m', [['quote']]),
      keybind('comma', [['comma']]),
      keybind('period', [['period']]),
      keybind('slash', [['delete_or_backspace']]),
    ],
  },
];

export const symbolsLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
