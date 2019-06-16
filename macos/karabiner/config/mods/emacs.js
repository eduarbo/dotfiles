const { remap } = require('../../utils');
const { emacsKeyBindingsException } = require('../../app-identifiers');

const mandatoryMods = ['control'];
const optionalMods = ['caps_lock'];

const remapIf = (fromKey, toKey, options) => remap([fromKey, mandatoryMods, optionalMods], toKey, {
  conditions: [{
    type: 'frontmost_application_unless',
    bundle_identifiers: emacsKeyBindingsException,
  }],
  ...options,
});

module.exports = {
  title: 'Emacs and shell style key bindings (@eduarbo)',
  rules: [
    {
      description: 'Delete bindings',
      manipulators: [
        remapIf('u', [
          ['left_arrow', ['control', 'left_shift']],
          ['delete_or_backspace'],
          ['vk_none'],
        ], {
          description: 'Ctrl + U to delete backward from point to the beginning of line.',
        }),
        remapIf('w', [['delete_or_backspace', ['option']]], {
          description: 'Ctrl + W to delete word behind point',
        }),
        remapIf('d', [['delete_forward']], {
          description: 'Ctrl + D to forward delete',
        }),
      ],
    },
    {
      description: 'Ctrl + B/F to move between words',
      manipulators: [
        remapIf('b', [['left_arrow', ['option']]], {
          description: 'Ctrl + W to delete word behind point',
        }),
        remapIf('f', [['right_arrow', ['option']]], {
          description: 'Ctrl + W to delete word behind point',
        }),
      ],
    },
  ],
};
