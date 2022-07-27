import * as lib from '../../lib';
import { remapToLayer } from '../../lib';
import type { ComplexModifications, FromKeyCodeTuple, ToKeyCodeTuple } from '../../lib';

const lazyModTap = (
  fromTuple: FromKeyCodeTuple,
  toTuples: ToKeyCodeTuple[],
  toTuplesOnTap: ToKeyCodeTuple[],
) => lib.lazyModTap(fromTuple, toTuples, toTuplesOnTap);

const rules = [
  {
    description: 'Thumb cluster',
    manipulators: [
      // L Command -> Sticky L Shift
      // remapToStickyModifier(['left_command', null, ['any']], ['right_shift']),
      lazyModTap(['left_command', null, ['any']], [['right_shift']], [['spacebar']]),

      // R Command -> Sticky R Shift
      // remapToStickyModifier(['right_command', null, ['any']], ['left_shift']),
      lazyModTap(['right_command', null, ['any']], [['left_shift']], [['escape']]),

      // Spacebar -> SUPER | Spacebar
      // TODO Make it sticky
      remapToLayer(['spacebar', null, ['any']], 'SUPER'),

      // L Option -> MEH
      remapToLayer(['left_option', null, ['any']], 'MEH'),
      remapToLayer(['right_option', null, ['any']], 'MEH'),
    ],
  },
];

export const base: ComplexModifications = { title: 'Main layer', rules };
