import { modTap, remap, remapToStickyModifier, games } from '../../lib';
import type { ComplexModifications, ManipulatorOptions } from '../../lib';

const LAYER = 'BASE';

const manipulatorOptions: ManipulatorOptions = {
  conditions: [
    {
      type: 'frontmost_application_unless',
      bundle_identifiers: games,
    },
  ],
};

const rules = [
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      // L Command + Spacebar -> emojis 👾
      modTap(
        ['spacebar', ['left_shift'], []],
        [['left_shift']],
        [['spacebar', ['right_control', 'right_command']]],
      ),

      // Spacebar -> Sticky fn
      remapToStickyModifier(['spacebar', null, ['any']], ['fn'], {
        toOptions: {
          lazy: true,
        },
        manipulatorOptions,
      }),

      // L Command -> L Shift on hold, SPACE on tap
      modTap(['left_command', null, ['any']], [['left_shift']], [['spacebar']], {
        setVariables: {
          LEFT_SHIFT: { to: true, to_after_key_up: false },
        },
        manipulatorOptions,
      }),

      // R Command -> R Shift (SYMBOLS) on hold, ESC on tap
      modTap(['right_command', null, ['any']], [['right_shift']], [['escape']], {
        setVariables: {
          RIGHT_SHIFT: { to: true, to_after_key_up: false },
        },
        manipulatorOptions,
      }),

      // FIXME My corne kbd switches CTRL and CMD keys for no apparent reason, could be an issue with QMK
      // L Control -> L Shift on hold, SPACE on tap
      modTap(['left_control', null, ['any']], [['left_shift']], [['spacebar']], {
        setVariables: {
          LEFT_SHIFT: { to: true, to_after_key_up: false },
        },
      }),

      // FIXME My corne kbd switches CTRL and CMD keys for no apparent reason, could be an issue with QMK
      // R Control -> R Shift (SYMBOLS) on hold, ESC on tap
      modTap(['right_control', null, ['any']], [['right_shift']], [['escape']], {
        setVariables: {
          RIGHT_SHIFT: { to: true, to_after_key_up: false },
        },
      }),

      // L Option -> SUPER
      remap(['left_option'], [['left_option', ['command', 'control']]], {
        manipulatorOptions,
      }),

      // R Option -> SUPER
      remap(['right_option'], [['right_option', ['command', 'control']]], {
        manipulatorOptions,
      }),

      // L+R Option -> HYPER
      remap(
        ['right_option', ['option', 'command', 'control']],
        [['left_shift', ['option', 'command', 'control']]],
      ),
      remap(
        ['left_option', ['option', 'command', 'control']],
        [['left_shift', ['option', 'command', 'control']]],
      ),
    ],
  },
];

export const baseLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
