import { modTap, remap, remapToStickyModifier } from '../../lib';
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

      // Spacebar -> Sticky fn
      remapToStickyModifier(['spacebar', null, ['any']], ['fn'], {
        toOptions: {
          lazy: true,
        },
      }),

      // L Command -> L Shift
      modTap(['left_command', null, ['any']], [['left_shift']], [['spacebar']], {
        setVariables: {
          LEFT_SHIFT: { to: true, to_after_key_up: false },
        },
      }),

      // R Command -> R Shift (SYMBOLS)
      modTap(['right_command', null, ['any']], [['right_shift']], [['escape']], {
        setVariables: {
          RIGHT_SHIFT: { to: true, to_after_key_up: false },
        },
      }),

      // FIXME My corne kbd switches CTRL and CMD keys for no apparent reason, could be an issue with QMK
      // L Command -> L Shift
      modTap(['left_control', null, ['any']], [['left_shift']], [['spacebar']], {
        setVariables: {
          LEFT_SHIFT: { to: true, to_after_key_up: false },
        },
      }),

      // R Command -> R Shift (SYMBOLS)
      modTap(['right_control', null, ['any']], [['right_shift']], [['escape']], {
        setVariables: {
          RIGHT_SHIFT: { to: true, to_after_key_up: false },
        },
      }),

      // L Option -> SUPER
      remap(['left_option'], [['left_option', ['command', 'control']]]),

      // R Option -> SUPER
      remap(['right_option'], [['right_option', ['command', 'control']]]),

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
