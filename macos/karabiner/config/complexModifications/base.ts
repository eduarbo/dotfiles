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
          HYPER: { to_after_key_up: false },
        },
      }),

      // L Command -> Sticky L Shift
      // remapToStickyModifier(['left_command', null, ['any']], ['right_shift']),
      modTap(['left_command', null, ['any']], [['left_shift']], [['spacebar']], {
        setVariables: {
          SHIFT: { to: true, to_after_key_up: false },
          HYPER: { to_after_key_up: false },
        },
      }),

      // R Command -> Sticky R Shift
      modTap(['right_command', null, ['any']], [], [['escape']], {
        setVariables: {
          SYMBOLS: { to: true, to_after_key_up: false },
          HYPER: { to_after_key_up: false },
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
