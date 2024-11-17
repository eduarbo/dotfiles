import { remap, ignoreKeebs } from '../../lib';
import type {
  Modifier,
  ModifierOptional,
  ToKeyCodeTuple,
  KeyCode,
  ComplexModifications,
} from '../../lib';

const LAYER = 'SYMBOLS';
const layerMods: Modifier[] = ['right_shift'];
const shiftedLayerMods: Modifier[] = ['left_shift', 'right_shift'];
const optionalMods: ModifierOptional[] = ['left_shift', 'right_command', 'right_control'];

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
      /// Top Row
      keybind('q', [['page_up']]), // ▲
      keybind('q', [['home']], { shifted: true }), // ⇱
      keybind('w', [['7']]),
      keybind('e', [['8']]),
      keybind('r', [['9']]),
      keybind('t', [['0']]),

      /// Home Row
      keybind('a', [['e', ['option']]]), // acento ´
      keybind('s', [['4']]),
      keybind('d', [['5']]),
      keybind('f', [['6']]),
      keybind('g', [['equal_sign']]), // =

      /// Bottom Row
      keybind('z', [['caps_lock']]), // CAPS_LOCK
      keybind('x', [['1']]),
      keybind('c', [['2']]),
      keybind('v', [['3']]),
      keybind('b', [['hyphen']]), // -
    ],
  },
  {
    description: `${LAYER} layer: Right hand - Symbols and arrows`,
    manipulators: [
      /// Top Row
      keybind('y', [['backslash']]), // \
      keybind('u', [['open_bracket']]), // [
      keybind('i', [['close_bracket']]), // ]
      keybind('o', [['slash']]), // /
      keybind('p', [['page_down']]), // ▼
      keybind('p', [['end']], { shifted: true }), // ⇲

      /// Home Row
      keybind('h', [['left_arrow']]), // ←
      keybind('j', [['down_arrow']]), // ↓
      keybind('k', [['up_arrow']]), // ↑
      keybind('l', [['right_arrow']]), // →
      keybind('semicolon', [['n', ['option']]]), // virgulilla (~)

      /// Bottom Row
      keybind('n', [['grave_accent_and_tilde']]), // `
      keybind('m', [['quote']]), // '

      // // NOTE Do not shift these, I want them to be available in the same layer as the numpad
      keybind('comma', [['comma']]),
      keybind('period', [['period']]),
      keybind('slash', [['delete_or_backspace']]),
    ],
  },
];

// FIXME: The built-in keyboard on MacBooks experiences ghosting with the keys `C`, `V`, `B`, `N`, `M`, `,`, `.`, and `/` when both Command keys are pressed together. A possible workaround is to remap the modifiers so that the Left CMD + Right CMD combo is reserved for infrequent keybinds.
// Suggestions:
// - Spacebar: Modtap - Space/Shift
// - Left CMD: Modtap - Enter/CMD
// - Right CMD: Modtap - Tab/Symbols (Right Shift)

export const symbolsLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
