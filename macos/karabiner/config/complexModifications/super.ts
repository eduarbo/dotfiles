import * as lib from '../../lib';
import { modTap } from '../../lib';
import type {
  ComplexModifications,
  KeyCode,
  Modifier,
  ToKeyCodeTuple,
  Manipulator,
  VariableCondition,
  RemapOptions,
  FromKeyCodeTuple,
} from '../../lib';

const ifSuper = {
  type: 'variable_if',
  name: 'SUPER',
  value: true,
};

const unlessMeh = {
  type: 'variable_unless',
  name: 'MEH',
  value: true,
};

const ifSymbols = {
  type: 'variable_if',
  name: 'SYMBOLS',
  value: true,
};

const unlessSymbols = {
  type: 'variable_unless',
  name: 'SYMBOLS',
  value: true,
};

const unlessShift = {
  type: 'variable_unless',
  name: 'SHIFT',
  value: true,
};

const remap = (
  fromTuple: FromKeyCodeTuple,
  toTuples: ToKeyCodeTuple[],
  options: RemapOptions = {},
): Manipulator =>
  lib.remap(fromTuple, toTuples, {
    manipulatorOptions: {
      conditions: [ifSuper, unlessMeh, unlessSymbols, unlessShift],
    },
    ...options,
  });

type StickyModifier = 'command' | 'option' | 'control' | 'shift';

const remapToStickyModifier = (
  fromKeyCode: KeyCode,
  toStickyModifiers: StickyModifier[],
  isRightSide: boolean,
): Manipulator => {
  const toModifiers = toStickyModifiers.map(
    (mod): Modifier => `${isRightSide ? 'right' : 'left'}_${mod}`,
  );
  const setVariables = toModifiers.reduce(
    (acc, mod) => ({
      ...acc,
      [mod.toUpperCase()]: { to: true, to_after_key_up: false },
    }),
    {},
  );

  const unlessOppositeMods = ['option', 'control', 'command'].map(
    (mod): VariableCondition => ({
      type: 'variable_unless',
      name: `${isRightSide ? 'left' : 'right'}_${mod}`.toUpperCase(),
      value: true,
    }),
  );

  return lib.remapToStickyModifier([fromKeyCode, null, ['any']], toModifiers, {
    setVariables,
    manipulatorOptions: {
      conditions: [ifSuper, unlessMeh, ...unlessOppositeMods],
    },
  });
};

const superMods: Modifier[] = ['option', 'command', 'control'];

const rules = [
  {
    description: 'SUPER Thumbs',
    manipulators: [
      // L Command -> Move to Right Desktop
      modTap(['left_command', null, []], [['left_shift']], [['left_arrow', ['right_control']]], {
        manipulatorOptions: {
          conditions: [ifSuper, unlessMeh],
        },
        setVariables: {
          SHIFT: { to: true, to_after_key_up: false },
        },
      }),

      // R Command -> Move to Left Desktop
      modTap(['right_command', null, []], [], [['right_arrow', ['right_control']]], {
        manipulatorOptions: {
          conditions: [ifSuper, unlessMeh],
        },
        setVariables: {
          SYMBOLS: { to: true, to_after_key_up: false },
        },
      }),
    ],
  },
  {
    description:
      'SUPER keys for Sticky Mods and quick access to the bindings I use most often in left hand',
    manipulators: [
      /// Top Row - Nav
      // Q key: reserved to switch the sound output
      remap(['q'], [['q', superMods]]),
      remap(['w'], [['open_bracket', ['right_command', 'right_shift']]]),
      remap(['e'], [['open_bracket', ['right_command']]]),
      remap(['r'], [['close_bracket', ['right_command']]]),
      remap(['t'], [['close_bracket', ['right_command', 'right_shift']]]),

      /// Home Row - Mods
      remap(['a'], [], {
        setVariables: { MEH: { to: true, to_after_key_up: false } },
        manipulatorOptions: {
          conditions: [ifSuper, unlessMeh, unlessSymbols],
        },
      }),
      remap(['a'], [['left_shift']], {
        setVariables: { MEH: { to: true, to_after_key_up: false } },
        manipulatorOptions: {
          conditions: [ifSuper, unlessMeh, ifSymbols],
        },
      }),
      remapToStickyModifier('s', ['option'], false),
      remapToStickyModifier('d', ['command'], false),
      remapToStickyModifier('f', ['control'], false),
      remapToStickyModifier('g', ['shift', 'option', 'command', 'control'], false),

      /// Bottom Row
      // Z key: reserved to toggle Mic with HammerSpoon 🔨🥄
      remap(['z'], [['z', superMods]]),
      remap(['x'], [['up_arrow', ['control']]]),
      remap(['c'], [['grave_accent_and_tilde', ['command']]]),
      remap(['v'], [['tab', ['shift', 'command']]], {
        manipulatorOptions: {
          conditions: [ifSuper, unlessMeh, ifSymbols],
        },
      }),
      remap(['v'], [['tab', ['command']]]),
      remap(['b'], [['down_arrow', ['control']]]),
    ],
  },
  {
    description:
      'SUPER keys for Sticky Mods and quick access to the bindings I use most often in right hand',
    manipulators: [
      /// Top Row
      // P key: reserved for Alfred's Universal Access
      remap(['y'], [['y', superMods]]),
      // U key: reserved to launch 1p quick access
      remap(['u'], [['u', superMods]]),
      // I key: reserved to launch Alfred
      remap(['i'], [['i', superMods]]),
      // O key: reserved for Alfred's snippets
      remap(['o'], [['o', superMods]]),
      // P key: avalilable
      remap(['p'], [['p', superMods]]),

      /// Home Row - Mods
      remapToStickyModifier('h', ['shift', 'option', 'command', 'control'], true),
      remapToStickyModifier('j', ['control'], true),
      remapToStickyModifier('k', ['command'], true),
      remapToStickyModifier('l', ['option'], true),
      remap(['semicolon'], [], { setVariables: { MEH: { to: true, to_after_key_up: false } } }),

      /// Bottom Row
      remap(['n'], [['home']]),
      remap(['m'], [['page_down']]),
      remap(['comma'], [['page_up']]),
      remap(['period'], [['end']]),
      // / key: available
      remap(['slash'], [['slash', superMods]]),
    ],
  },
];

export const supr: ComplexModifications = { title: 'Super layer', rules };
