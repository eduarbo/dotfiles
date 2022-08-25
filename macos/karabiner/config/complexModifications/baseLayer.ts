import { modTap, remap } from '../../lib';
import type { ComplexModifications } from '../../lib';

const LAYER = 'BASE';

const rules = [
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      // L Command + Spacebar -> emojis 👾
      modTap(
        ['spacebar', ['left_shift'], []],
        [['left_shift']],
        [['spacebar', ['right_control', 'right_command']]],
        {
          setVariables: {
            MODS_NAV: { to: true, to_after_key_up: false },
          },
        },
      ),

      // Spacebar -> MODS_NAV | Spacebar
      // TODO Make it sticky
      remap(['spacebar', null, ['any']], [], {
        setVariables: {
          MODS_NAV: { to: true, to_after_key_up: false },
        },
      }),

      // L Command -> L Shift
      modTap(['left_command', null, ['any']], [['left_shift']], [['spacebar']], {
        setVariables: {
          LEFT_SHIFT: { to: true, to_after_key_up: false },
        },
      }),

      // R Command -> R Shift
      modTap(['right_command', null, ['any']], [['right_shift']], [['escape']], {
        setVariables: {
          RIGHT_SHIFT: { to: true, to_after_key_up: false },
        },
      }),

      // L Option -> SUPER
      remap(['left_option', null, ['any']], [['left_option', ['command', 'control']]]),

      // R Option -> HYPER
      remap(['right_option', null, ['any']], [['right_option', ['shift', 'command', 'control']]]),
    ],
  },
];

export const baseLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
