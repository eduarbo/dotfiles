import R from 'ramda';
import { modTap as _modTap, remap as _remap, CIV5 } from '../../lib';
import type { ComplexModifications, ManipulatorOptions } from '../../lib';

const LAYER = 'Civ V';

const manipulatorOptions: ManipulatorOptions = {
  conditions: [
    {
      type: 'frontmost_application_if',
      bundle_identifiers: [CIV5],
    },
  ],
};

const toOptions = {
  lazy: true,
};

const remap = R.partialRight(_remap, [{ manipulatorOptions }]);

const modTap = R.partialRight(_modTap, [{ manipulatorOptions, toOptions }]);

const rules = [
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      modTap(['right_command'], [['right_command']], [['return_or_enter', ['left_shift']]]),
    ],
  },
  {
    description: `${LAYER} layer: Custom QWERTY`,
    manipulators: [
      remap(['q', [], []], [['delete_or_backspace']]),
      remap(['w', [], []], [['home']]),
      remap(['r', [], []], [['end']]),
      remap(['t', [], []], [['u']]),
      remap(['s', [], []], [['comma']]),
      remap(['d', [], []], [['f3']]),
      remap(['f', [], []], [['period']]),
      remap(['g', [], []], [['m']]),
      remap(['z', [], []], [['f']]),
      remap(['x', [], []], [['h']]),
      remap(['c', [], []], [['s']]),
      remap(['v', [], []], [['a', ['control']]]),
    ],
  },
];

export const civLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
