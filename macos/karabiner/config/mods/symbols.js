const { remap } = require('../../utils');

const symbolsMods = ['left_shift'];
const shiftedNumbersRow = 'pqwertyuio'.split('');
const numbersRow = ['semicolon', ...'asdfghjkl'.split('')];
const optionalMods = ['caps_lock', 'command', 'control', 'option'];
const remapToLayer = (fromKey, toKey) => remap([fromKey, symbolsMods, optionalMods], toKey);

module.exports = {
  title: 'Symbols layer',
  rules: [
    {
      description: 'Shifted numbers in upper row, numbers in home row, other symbols in lower row',
      manipulators: [
        // Upper row
        shiftedNumbersRow.map((key, num) => remapToLayer(key, [[num, ['left_shift']]])),

        // Home row
        numbersRow.map((key, num) => remapToLayer(key, [[num]])),
        remapToLayer('quote', [['equal_sign', ['left_shift']]]),
        remapToLayer('caps_lock', [['hyphen']]),

        // Lower row
        remapToLayer('left_shift', [['f13']]),
        remapToLayer('grave_accent_and_tilde', [['f13']]),
        remapToLayer('z', [['1', ['left_option']]]),
        remapToLayer('x', [['backslash', ['left_shift']]]),
        remapToLayer('c', [['hyphen', ['left_shift']]]),
        remapToLayer('v', [['equal_sign']]),
        remapToLayer('b', [['backslash']]),
        remapToLayer('n', [['n', ['left_option']]]),
        remapToLayer('m', [['e', ['left_option']]]),
        remapToLayer('comma', [['comma']]),
        remapToLayer('period', [['period']]),
        remapToLayer('slash', [['slash']]),
        remapToLayer('right_shift', [['slash', ['left_shift', 'left_option']]]),
      ],
    },
  ],
};
