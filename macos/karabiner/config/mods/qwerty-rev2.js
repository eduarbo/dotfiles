const { modTap, remap } = require('../../utils');

module.exports = {
  title: 'Qwerty layer Rev2',
  rules: [
    {
      description: 'Thumb cluster',
      manipulators: [
        modTap(['spacebar'], [['right_command']], [['spacebar']]),
        modTap(['right_command'], [['right_shift']], [['escape']]),
        modTap(['left_command'], [['left_shift']], [['return_or_enter']]),
        modTap(['right_option'], [['right_option']], [['escape']]),
        modTap(['left_option'], [['right_control']], [['return_or_enter']]),
      ],
    },
    {
      description: 'Custom Qwerty (Rev2)',
      manipulators: [
        remap(['delete_or_backspace'], [['f13']]),
        remap(['open_bracket'], [['delete_or_backspace']]),
        remap(['caps_lock'], [['grave_accent_and_tilde']]),
        remap(['quote'], [['quote']]),
        remap(['left_shift'], [['open_bracket']]),
        remap(['right_shift'], [['close_bracket']]),
      ],
    },
  ],
};
