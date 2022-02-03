import {
  remap,
  complexModifications,
  EMACS_KEY_BINDINGS_EXCEPTION,
} from '../../lib';
import type { ManipulatorOptions, ToKeyCodeTuple, KeyCode } from '../../lib';

const keybind = (fromKeyCode: KeyCode, toTuples: ToKeyCodeTuple[], options: ManipulatorOptions) =>
  remap([fromKeyCode, ['control'], ['caps_lock']], toTuples, {
    conditions: [
      {
        type: 'frontmost_application_unless',
        bundle_identifiers: EMACS_KEY_BINDINGS_EXCEPTION,
      },
    ],
    ...options,
  });

export const emacs = complexModifications('Emacs and shell style key bindings', [
  {
    description: 'Delete bindings',
    manipulators: [
      keybind(
        'u',
        [['left_arrow', ['control', 'left_shift']], ['delete_or_backspace'], ['vk_none']],
        {
          description: 'Ctrl + U to delete backward from point to the beginning of line.',
        },
      ),
      keybind('w', [['delete_or_backspace', ['option']]], {
        description: 'Ctrl + W to delete word behind point',
      }),
      keybind('d', [['delete_forward']], {
        description: 'Ctrl + D to forward delete',
      }),
    ],
  },
  {
    description: 'Ctrl + B/F to move between words',
    manipulators: [
      keybind('b', [['left_arrow', ['option']]], {
        description: 'Ctrl + W to delete word behind point',
      }),
      keybind('f', [['right_arrow', ['option']]], {
        description: 'Ctrl + W to delete word behind point',
      }),
    ],
  },
]);
