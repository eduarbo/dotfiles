import { remap, EMACS_KEY_BINDINGS_EXCEPTION } from '../../lib';
import type { ManipulatorOptions, ToKeyCodeTuple, KeyCode, ComplexModifications } from '../../lib';

const keybind = (
  fromKeyCode: KeyCode,
  toTuples: ToKeyCodeTuple[],
  manipulatorOptions: ManipulatorOptions,
) =>
  remap([fromKeyCode, ['control'], ['caps_lock']], toTuples, {
    manipulatorOptions: {
      conditions: [
        {
          type: 'frontmost_application_unless',
          bundle_identifiers: EMACS_KEY_BINDINGS_EXCEPTION,
        },
      ],
      ...manipulatorOptions,
    },
  });

const rules = [
  {
    description: 'Emacs keybindings: Delete',
    manipulators: [
      keybind('u', [['delete_or_backspace', ['command']]], {
        description: 'Ctrl + U to delete text behind the cursor',
      }),
      keybind('w', [['delete_or_backspace', ['option']]], {
        description: 'Ctrl + W to delete word behind the cursor',
      }),
      keybind('d', [['delete_forward', ['option']]], {
        description: 'Ctrl + D to delete word in front of the cursor',
      }),
    ],
  },
  {
    description: 'Emacs keybindings: Word nav - Ctrl + B/F to move between words',
    manipulators: [
      keybind('b', [['left_arrow', ['option']]], {
        description: 'Ctrl + B to move backward a word',
      }),
      keybind('f', [['right_arrow', ['option']]], {
        description: 'Ctrl + F to move forward a word',
      }),
    ],
  },
];

export const emacsKeybindings: ComplexModifications = {
  title: 'Emacs and shell style key bindings',
  rules,
};
