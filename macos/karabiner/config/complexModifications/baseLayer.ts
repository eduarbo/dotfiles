import { modTap, remap, ignoreKeebs } from '../../lib';
import type { ComplexModifications } from '../../lib';

const LAYER = 'BASE';

const manipulatorOptions = {
  conditions: ignoreKeebs,
};

const toOptions = {
  lazy: true,
};

const rules = [
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      // L Command -> L Shift on hold, SPACE on tap
      modTap(['left_command', null, ['any']], [['left_shift']], [['spacebar']], {
        manipulatorOptions,
        toOptions,
      }),

      // R Command -> R Shift (SYMBOLS) on hold, Tab on tap
      modTap(['right_command', null, ['any']], [['right_shift']], [['tab']], {
        manipulatorOptions,
        toOptions,
      }),

      // Spacebar -> R Command on hold, Enter on tap
      modTap(['spacebar', null, ['any']], [['right_command']], [['return_or_enter']], {
        manipulatorOptions,
        toOptions,
      }),

      // L Option -> SUPER on hold, ESC on tap
      modTap(
        ['left_option', null, ['any']],
        [['left_option', ['left_command', 'left_control']]],
        [['escape']],
        {
          manipulatorOptions,
          toOptions,
        },
      ),

      // R Option -> R Control on hold, F16 on tap
      modTap(['right_option', null, ['any']], [['right_control']], [['f16']], {
        manipulatorOptions,
        toOptions,
      }),
    ],
  },
  {
    description: `${LAYER} layer: Custom QWERTY`,
    manipulators: [
      // / -> DEL
      remap(['slash'], [['delete_or_backspace']], {
        manipulatorOptions,
      }),
    ],
  },
];

export const baseLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
