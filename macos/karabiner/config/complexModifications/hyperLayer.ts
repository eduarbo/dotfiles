import { fromAny, ignoreKeebs, manipulator, toFromEvent } from '../../lib/index.js';
import type { ComplexModifications } from '../../lib/index.js';
import { SUPER_LAYER_VARIABLE, SYMBOLS_LAYER_VARIABLE } from './layerVariables.js';

const LAYER = 'HYPER';

const rules = [
  {
    description: `${LAYER} layer: Pass through keys with Control+Option+Command+Shift`,
    manipulators: [
      manipulator(fromAny(['key_code', undefined, ['any']]), {
        to: [
          toFromEvent({
            modifiers: ['left_control', 'left_option', 'left_command', 'right_shift'],
          }),
        ],
        conditions: [
          ...ignoreKeebs,
          { type: 'variable_if', name: SUPER_LAYER_VARIABLE, value: 1 },
          { type: 'variable_if', name: SYMBOLS_LAYER_VARIABLE, value: 1 },
        ],
      }),
    ],
  },
];

export const hyperLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
