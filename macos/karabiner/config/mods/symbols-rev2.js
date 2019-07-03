const { remap } = require('../../utils');
const { corneKeyboard } = require('../devices');

const mandatoryMods = ['left_shift'];
const optionalMods = ['caps_lock', 'command', 'control', 'option'];
const shiftedNumbersRow = 'pqwertyuio'.split('');
const numbersRow = ['semicolon', ...'asdfghjkl'.split('')];
const remapToLayer = (keyCode, toKey) => remap([keyCode, mandatoryMods, optionalMods], toKey, {
  // conditions: [{
  //   type: 'device_unless',
  //   identifiers: [corneKeyboard],
  // }],
});

module.exports = {
  title: 'Symbols layer',
  rules: [
    {
      description: 'Shifted numbers in upper row, numbers in home row, other symbols in lower row',
      manipulators: [
        // Upper row
        ...shiftedNumbersRow.map((key, num) => remapToLayer(key, [[num.toString(), ['left_shift']]])),

        // Home row
        ...numbersRow.map((key, num) => remapToLayer(key, [[num.toString()]])),
        remapToLayer('caps_lock', [['f13']]),
        remapToLayer('quote', [['backslash', ['left_shift']]]),

        // Lower row
        remapToLayer('left_shift', [['1', ['left_option']]]),
        remapToLayer('grave_accent_and_tilde', [['1', ['left_option']]]),
        remapToLayer('z', [['hyphen', ['left_shift']]]),
        remapToLayer('x', [['hyphen']]),
        remapToLayer('c', [['equal_sign', ['left_shift']]]),
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
