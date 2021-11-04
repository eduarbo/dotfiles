const utils = require('../../utils');

const remap = (fromKey, toKey) => utils.remap(fromKey, toKey);
const modTap = (fromKey, toKey, toKeyOnTap) => utils.modTap(fromKey, toKey, toKeyOnTap);

module.exports = {
  title: 'Alternative Qwerty layer',
  rules: [
    {
      description: 'Thumb cluster',
      manipulators: [
        modTap(['spacebar'], [['right_command']], [['return_or_enter']]),
        modTap(['right_command'], [['right_shift']], [['escape']]),
        modTap(['left_command'], [['left_shift']], [['spacebar']]),

        modTap(['right_option'], [['right_option']], [['grave_accent_and_tilde']]),
        modTap(['left_option'], [['right_control']], [['quote']]),
      ],
    },
    {
      description: 'Custom Qwerty Tweaks',
      manipulators: [
        // Swap CMD+LSHIFT <-> CMD+` to swich between apps/windows with one hand
        remap(['left_shift', ['command']], [['grave_accent_and_tilde', ['left_command']]]),
        remap(['open_bracket', ['command']], [['delete_or_backspace', ['left_command']]]),

        // Quick access to F13 for the built-in macbook keyboard
        remap(['delete_or_backspace'], [['f13']]),
      ],
    },
    {
      description: 'Custom Qwerty',
      manipulators: [
        // [ -> '
        remap(['open_bracket'], [['quote']]),
        // ' -> ]
        remap(['quote'], [['close_bracket']]),
        // CAPS -> [
        remap(['caps_lock'], [['open_bracket']]),
        // LSHFIT -> DEL
        remap(['left_shift'], [['delete_or_backspace']]),
        // RSHIFT -> `
        remap(['right_shift'], [['grave_accent_and_tilde']]),
      ],
    },
  ],
};
