import { remap, games } from '../../lib';
import type { ComplexModifications, ManipulatorOptions } from '../../lib';

const LAYER = 'GAMING';

const manipulatorOptions: ManipulatorOptions = {
  conditions: [
    {
      type: 'frontmost_application_if',
      bundle_identifiers: games,
    },
  ],
};

const rules = [
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      // L Command -> L Shift
      remap(['left_command'], [['left_shift']], {
        manipulatorOptions,
      }),

      // R Command -> R Command
      remap(['right_command'], [['right_command']], {
        manipulatorOptions,
      }),

      // L Option -> SUPER
      remap(['left_option'], [['left_control']], {
        manipulatorOptions,
      }),

      // R Option -> L Option
      remap(['right_option'], [['left_option']], {
        manipulatorOptions,
      }),
    ],
  },
];

export const gamingLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
