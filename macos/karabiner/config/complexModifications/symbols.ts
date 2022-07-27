import * as lib from '../../lib';
import { lazyModTap } from '../../lib';
import type { ToKeyCodeTuple, Modifier, KeyCode, ComplexModifications } from '../../lib';

const mandatoryModifiers: Modifier[] = ['left_shift'];

const remap = (fromKeyCode: KeyCode, toTuples: ToKeyCodeTuple[]) =>
  lib.remap([fromKeyCode, mandatoryModifiers, ['any']], toTuples);

const rules = [
  {
    description: 'Numpad in left hand',
    manipulators: [
      /// Top Row
      remap('q', [['backslash']]), // \
      remap('w', [['7']]),
      remap('e', [['8']]),
      remap('r', [['9']]),
      remap('t', [['0']]),

      /// Home Row
      remap('a', [['hyphen']]), // -
      remap('s', [['4']]),
      remap('d', [['5']]),
      remap('f', [['6']]),
      remap('g', [['equal_sign']]), // =

      /// Bottom Row
      remap('z', [['open_bracket']]), // [
      remap('x', [['1']]),
      remap('c', [['2']]),
      remap('v', [['3']]),
      remap('b', [['close_bracket']]), // ]

      /// Thumbs
      // Spacebar -> HYPER | F13
      lazyModTap(
        ['spacebar', mandatoryModifiers, []],
        [['right_shift', ['right_option', 'right_control', 'right_command']]],
        [['f13']],
      ), // F13 is reserved for Alfred's Clipboard History
    ],
  },
  {
    description: 'Symbols and arrows in right hand',
    manipulators: [
      /// Top Row
      remap('y', [['quote']]), // '
      remap('u', [['grave_accent_and_tilde']]), // `
      remap('i', [['return_or_enter']]), // RET
      remap('o', [['tab']]), // TAB
      // FIXME Find out a way to port the X-Case functionality from QMK to Karabiner
      remap('p', [['vk_none']]), // P key: reserverd for XCase in my crkbd

      /// Home Row
      remap('h', [['left_arrow']]), // ←
      remap('j', [['down_arrow']]), // ↓
      remap('k', [['up_arrow']]), // ↑
      remap('l', [['right_arrow']]), // →
      // FIXME Find out a way to port the CapsWord functionality from QMK to Karabiner
      remap('semicolon', [['caps_lock']]), // using CAPS_LOCK as fallback

      /// Bottom Row
      remap('n', [['e', ['right_option']]]),
      remap('m', [['delete_or_backspace']]), // ⌫
      // Do not shift these, I want them to be available in the same layer as the numpad
      remap('comma', [['comma']]),
      remap('period', [['period']]),
      remap('slash', [['slash']]),
    ],
  },
];

export const symbols: ComplexModifications = { title: 'Symbols layer', rules };
