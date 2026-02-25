import { modTap, remap, CIV5 } from '../../lib/index.js';
import type { ComplexModifications, ManipulatorOptions, FromKeyCodeTuple, ToKeyCodeTuple, RemapOptions } from '../../lib/index.js';

const LAYER = 'Civ V';

const manipulatorOptions: ManipulatorOptions = {
  conditions: [
    {
      type: 'frontmost_application_if',
      bundle_identifiers: [CIV5],
    },
  ],
};

const toOptions = { lazy: true };

const civRemap = (fromTuple: FromKeyCodeTuple, toTuples: ToKeyCodeTuple[], extra: RemapOptions = {}) =>
  remap(fromTuple, toTuples, { manipulatorOptions, ...extra });

const civModTap = (fromTuple: FromKeyCodeTuple, toTuples: ToKeyCodeTuple[], toTuplesOnTap: ToKeyCodeTuple[]) =>
  modTap(fromTuple, toTuples, toTuplesOnTap, { manipulatorOptions, toOptions });

const rules = [
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      civModTap(['right_command'], [['right_command']], [['return_or_enter', ['left_shift']]]),
    ],
  },
  {
    description: `${LAYER} layer: Custom QWERTY`,
    manipulators: [
      civRemap(['q', [], []], [['delete_or_backspace']]),
      civRemap(['w', [], []], [['home']]),
      civRemap(['r', [], []], [['end']]),
      civRemap(['t', [], []], [['u']]),
      civRemap(['s', [], []], [['comma']]),
      civRemap(['d', [], []], [['f3']]),
      civRemap(['f', [], []], [['period']]),
      civRemap(['g', [], []], [['m']]),
      civRemap(['z', [], []], [['f']]),
      civRemap(['x', [], []], [['h']]),
      civRemap(['c', [], []], [['s']]),
      civRemap(['v', [], []], [['a', ['control']]]),
    ],
  },
];

export const civLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
