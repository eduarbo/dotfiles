import { remap, modTap } from '../../lib';
import type { Modifier, ToKeyCodeTuple, KeyCode, ComplexModifications } from '../../lib';

const LAYER = 'SYMBOLS';
const layerMods: Modifier[] = ['right_shift'];
const optionalMods: Modifier[] = ['any'];

const keybind = (fromKeyCode: KeyCode, toTuples: ToKeyCodeTuple[]) =>
  remap([fromKeyCode, layerMods, optionalMods], toTuples);

const rules = [
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      // L Command -> Shift + Spacebar
      modTap(
        ['left_command', layerMods, optionalMods],
        [['left_shift', ['right_shift']]],
        [['spacebar', ['shift']]],
      ),

      // Spacebar -> clipboard history
      modTap(['spacebar', layerMods, optionalMods], [], [['f13']], {
        setVariables: {
          FN: { to: true, to_after_key_up: false },
        },
      }),
    ],
  },
  {
    description: `${LAYER} layer: Left hand - Numpad`,
    manipulators: [
      /// Top Row
      keybind('q', [['backslash']]), // \
      keybind('w', [['7']]),
      keybind('e', [['8']]),
      keybind('r', [['9']]),
      keybind('t', [['0']]),

      /// Home Row
      keybind('a', [['hyphen']]), // -
      keybind('s', [['4']]),
      keybind('d', [['5']]),
      keybind('f', [['6']]),
      keybind('g', [['equal_sign']]), // =

      /// Bottom Row
      keybind('z', [['open_bracket']]), // [
      keybind('x', [['1']]),
      keybind('c', [['2']]),
      keybind('v', [['3']]),
      keybind('b', [['close_bracket']]), // ]
    ],
  },
  {
    description: `${LAYER} layer: Right hand - Symbols and arrows`,
    manipulators: [
      /// Top Row
      keybind('y', [['quote']]), // '
      keybind('u', [['grave_accent_and_tilde']]), // `
      keybind('i', [['return_or_enter']]), // RET
      keybind('o', [['tab']]), // TAB
      // FIXME Find out a way to port the X-Case functionality from QMK to Karabiner
      keybind('p', [['delete_forward']]), // using ⌦ as fallback

      /// Home Row
      keybind('h', [['left_arrow']]), // ←
      keybind('j', [['down_arrow']]), // ↓
      keybind('k', [['up_arrow']]), // ↑
      keybind('l', [['right_arrow']]), // →
      // FIXME Find out a way to port the CapsWord functionality from QMK to Karabiner
      keybind('semicolon', [['caps_lock']]), // using CAPS_LOCK as fallback

      /// Bottom Row
      keybind('n', [['e', ['option']]]), // accent
      keybind('m', [['delete_or_backspace']]), // ⌫
      // Do not shift these, I want them to be available in the same layer as the numpad
      keybind('comma', [['comma']]),
      keybind('period', [['period']]),
      keybind('slash', [['slash']]),
    ],
  },
];

export const symbolsLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
