import { modTap, remap } from '../../lib';
import type { ComplexModifications } from '../../lib';

const rules = [
  {
    description: 'Thumb cluster',
    manipulators: [
      // Spacebar -> SUPER | Spacebar
      // TODO Make it sticky
      remap(['spacebar'], [], {
        setVariables: {
          SUPER: { to: true, to_after_key_up: false },
        },
      }),

      // L Command -> L Shift
      modTap(['left_command', null, ['any']], [['left_shift']], [['spacebar']], {
        setVariables: {
          SHIFT: { to: true, to_after_key_up: false },
        },
      }),

      // L Command + Spacebar -> 👾
      modTap(
        ['spacebar', ['left_shift'], []],
        [['left_shift']],
        [['spacebar', ['right_control', 'right_command']]],
        {
          setVariables: {
            SHIFT: { to: true, to_after_key_up: false },
            SUPER: { to: true, to_after_key_up: false },
          },
          manipulatorOptions: {
            conditions: [
              {
                type: 'variable_unless',
                name: 'MEH',
                value: true,
              },
            ],
          },
        },
      ),

      // R Command -> R Shift
      modTap(['right_command', null, ['any']], [], [['escape']], {
        setVariables: {
          SYMBOLS: { to: true, to_after_key_up: false },
        },
      }),

      // L Option -> MEH
      remap(['left_option', null, ['any']], [], {
        setVariables: { MEH: { to: true, to_after_key_up: false } },
      }),
      remap(['right_option', null, ['any']], [], {
        setVariables: { MEH: { to: true, to_after_key_up: false } },
      }),
    ],
  },
];

export const base: ComplexModifications = { title: 'Main layer', rules };
