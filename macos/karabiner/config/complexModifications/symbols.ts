import { remap } from '../../lib';
import type { ToKeyCodeTuple, KeyCode, ComplexModifications } from '../../lib';

const remapToLayer = (fromKeyCode: KeyCode, toTuples: ToKeyCodeTuple[]) =>
  remap([fromKeyCode, ['right_shift'], ['any']], toTuples);

const rules = [
    {
      description: 'Numpad in left hand, symbols and arrows in right hand',
      manipulators: [
        // Numbers
        remapToLayer('w', [['7']]),
        remapToLayer('e', [['8']]),
        remapToLayer('r', [['9']]),
        remapToLayer('s', [['4']]),
        remapToLayer('d', [['5']]),
        remapToLayer('f', [['6']]),
        remapToLayer('x', [['1']]),
        remapToLayer('c', [['2']]),
        remapToLayer('v', [['3']]),
        remapToLayer('t', [['0']]),

        // Shifted numbers
        // !
        remapToLayer('y', [['1', ['shift']]]),
        // @
        remapToLayer('u', [['2', ['shift']]]),
        // #
        remapToLayer('i', [['3', ['shift']]]),
        // $
        remapToLayer('o', [['4', ['shift']]]),
        // %
        remapToLayer('p', [['5', ['shift']]]),
        // ^
        remapToLayer('open_bracket', [['6', ['shift']]]),
        // &
        remapToLayer('n', [['7', ['shift']]]),
        // *
        remapToLayer('m', [['8', ['shift']]]),
        // (
        remapToLayer('caps_lock', [['9', ['shift']]]),
        // )
        remapToLayer('quote', [['0', ['shift']]]),
        // +
        remapToLayer('q', [['equal_sign', ['shift']]]),
        // =
        remapToLayer('g', [['equal_sign']]),
        // -
        remapToLayer('a', [['hyphen']]),
        // _
        remapToLayer('z', [['hyphen', ['shift']]]),
        // TODO
        // remapToLayer('b', [[]]),
        // TODO FN Layer maybe?
        // remapToLayer('semicolon', [[]]),

        // Arrows
        // ←
        remapToLayer('h', [['left_arrow']]),
        // ↓
        remapToLayer('j', [['down_arrow']]),
        // ↑
        remapToLayer('k', [['up_arrow']]),
        // →
        remapToLayer('l', [['right_arrow']]),

        // Do not shift these
        remapToLayer('comma', [['comma']]),
        remapToLayer('period', [['period']]),
        remapToLayer('slash', [['slash']]),
        remapToLayer('right_shift', [['backslash']]),
      ],
    },
  ];

export const symbols: ComplexModifications = { title: 'Symbols layer', rules };
