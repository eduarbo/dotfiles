const { remap } = require('../../utils');

const mandatoryMods = ['left_shift'];
const optionalMods = ['caps_lock', 'command', 'control', 'option'];
const shiftedNumbersRow = 'pqwertyuio'.split('');
const numbersRow = ['semicolon', ...'asdfghjkl'.split('')];
const remapToLayer = (keyCode, toKey) => remap([keyCode, mandatoryMods, optionalMods], toKey);

module.exports = {
  title: 'Symbols layer',
  rules: [
    {
      description: 'Shifted numbers in upper row, numbers in home row, other symbols in lower row',
      manipulators: [
        // Upper row

        // !@#$%^&*()
        ...shiftedNumbersRow.map((key, num) => remapToLayer(key, [[num.toString(), ['left_shift']]])),
        remapToLayer('tab', [['f13']]),

        // Home row

        ...numbersRow.map((key, num) => remapToLayer(key, [[num.toString()]])),
        // -
        remapToLayer('caps_lock', [['hyphen']]),
        // +
        remapToLayer('quote', [['equal_sign', ['left_shift']]]),

        // Lower row

        // _
        remapToLayer('z', [['hyphen', ['left_shift']]]),
        // \
        remapToLayer('x', [['backslash']]),
        // |
        remapToLayer('c', [['backslash', ['left_shift']]]),
        // =
        remapToLayer('v', [['equal_sign']]),
        // ~ (tilde)
        remapToLayer('n', [['n', ['option']]]),
        // ’ (accent)
        remapToLayer('m', [['close_bracket', ['option', 'shift']]]),
        // emojis
        remapToLayer('b', [['spacebar', ['control', 'command']]]),

        // Do not shift these
        remapToLayer('comma', [['comma']]),
        remapToLayer('period', [['period']]),
        remapToLayer('slash', [['slash']]),
      ],
    },
  ],
};
