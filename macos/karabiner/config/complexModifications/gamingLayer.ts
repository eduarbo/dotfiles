import { remap, GAMES } from '../../lib/index.js';
import type { ComplexModifications, ManipulatorOptions } from '../../lib/index.js';

const LAYER = 'GAMING';

const manipulatorOptions: ManipulatorOptions = {
  conditions: [
    {
      type: 'frontmost_application_if',
      bundle_identifiers: GAMES,
    },
  ],
};

const rules = [
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      remap(['left_command'], [['left_shift']], { manipulatorOptions }),
      remap(['right_command'], [['right_command']], { manipulatorOptions }),
      remap(['left_option'], [['left_control']], { manipulatorOptions }),
      remap(['right_option'], [['left_option']], { manipulatorOptions }),
    ],
  },
];

export const gamingLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
