const { modTap, remap } = require('../../utils');

module.exports = {
  title: 'Qwerty layer (@eduarbo)',
  rules: [
    {
      description: 'Thumb cluster',
      manipulators: [
        modTap(['spacebar'], [['right_command']], [['return_or_enter']]),
        modTap(['right_command'], [['right_shift']], [['escape']]),
        modTap(['left_command'], [['left_shift']], [['spacebar']]),
        modTap(['right_option'], [['right_option']], [['0', ['left_shift']]]),
        modTap(['left_option'], [['right_control']], [['9', ['left_shift']]]),
      ],
    },
    {
      description: 'Custom Qwerty',
      manipulators: [
        remap(['delete_or_backspace'], [['f13']]),
        remap(['open_bracket'], [['delete_or_backspace']]),
        remap(['caps_lock'], [['open_bracket']]),
        remap(['quote'], [['close_bracket']]),
        remap(['left_shift'], [['grave_accent_and_tilde']]),
        remap(['right_shift'], [['quote']]),
      ],
    },
  ],
};
