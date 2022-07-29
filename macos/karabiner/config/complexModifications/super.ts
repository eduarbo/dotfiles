import * as lib from '../../lib';
import { modTap, toHyperKeyCodeTuple } from '../../lib';
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

const ifSuperOn = {
  type: 'variable_if',
  name: 'SUPER',
  value: true,
};

const remapToStickyModifier = (
  fromKeyCode: KeyCode,
  toModifier: 'command' | 'option' | 'control' | 'shift',
  isRightSide: boolean,
): Manipulator => {
  const toKeyCode: KeyCode = `${isRightSide ? 'right' : 'left'}_${toModifier}`;

  return lib.remapToStickyModifier([fromKeyCode, null, ['any']], [toKeyCode], {
    setVariables: { [toKeyCode.toUpperCase()]: { to: true, to_after_key_up: false } },
    manipulatorOptions: {
      conditions: [
        ifSuperOn,
        {
          type: 'variable_unless',
          name: 'HYPER',
          value: true,
        },
        {
          type: 'variable_unless',
          name: 'MEH',
          value: true,
        },
        ...['option', 'control', 'command'].map(
          (mod): VariableCondition => ({
            type: 'variable_unless',
            name: `${isRightSide ? 'left' : 'right'}_${mod}`.toUpperCase(),
            value: true,
          }),
        ),
      ],
    },
  });
};

const defaultOpts = {
  manipulatorOptions: {
    conditions: [ifSuperOn],
  },
};

const remap = (
  fromTuple: FromKeyCodeTuple,
  toTuples: ToKeyCodeTuple[],
  options: RemapOptions = {},
): Manipulator => lib.remap(fromTuple, toTuples, { ...defaultOpts, ...options });

const superMods: Modifier[] = ['option', 'command', 'control'];

const rules = [
  {
    description: 'SUPER Thumbs',
    manipulators: [
      // L Command -> HYPER | Move to Right Desktop
      modTap(
        ['left_command', null, []],
        [toHyperKeyCodeTuple],
        [['left_arrow', ['right_control']]],
        {
          manipulatorOptions: {
            conditions: [
              ifSuperOn,
              {
                type: 'variable_unless',
                name: 'MEH',
                value: true,
              },
            ],
          },
          setVariables: {
            SHIFT: { to: true, to_after_key_up: false },
            HYPER: { to: true, to_after_key_up: false },
          },
        },
      ),

      // R Command -> HYPER | Move to Left Desktop
      modTap(
        ['right_command', null, []],
        [toHyperKeyCodeTuple],
        [['right_arrow', ['right_control']]],
        {
          manipulatorOptions: {
            conditions: [
              ifSuperOn,
              {
                type: 'variable_unless',
                name: 'MEH',
                value: true,
              },
            ],
          },
          setVariables: {
            SYMBOLS: { to: true, to_after_key_up: false },
            HYPER: { to: true, to_after_key_up: false },
          },
        },
      ),

      // Spacebar + L Command -> HYPER | accent
      modTap(['spacebar', ['left_shift'], []], [toHyperKeyCodeTuple], [['e', ['right_option']]], {
        setVariables: {
          SUPER: { to: true, to_after_key_up: false },
          HYPER: { to: true, to_after_key_up: false },
        },
        manipulatorOptions: {
          conditions: [
            {
              type: 'variable_unless',
              name: 'MEH',
              value: true,
            },
          ],
        },
      }),

      // Spacebar + R Command -> HYPER | clipboard history
      modTap(['spacebar'], [toHyperKeyCodeTuple], [['f13']], {
        setVariables: {
          SUPER: { to: true, to_after_key_up: false },
          HYPER: { to: true, to_after_key_up: false },
        },
        manipulatorOptions: {
          conditions: [
            {
              type: 'variable_if',
              name: 'SYMBOLS',
              value: true,
            },
            {
              type: 'variable_unless',
              name: 'MEH',
              value: true,
            },
          ],
        },
      }),
    ],
  },
  {
    description:
      'SUPER keys for Sticky Mods and quick access to the bindings I use most often in left hand',
    manipulators: [
      /// Top Row - Nav
      remap(['q'], [['a', ['right_command', 'right_shift']]]),
      remap(['w'], [['open_bracket', ['right_command', 'right_shift']]]),
      remap(['e'], [['open_bracket', ['right_command']]]),
      remap(['r'], [['close_bracket', ['right_command']]]),
      remap(['t'], [['close_bracket', ['right_command', 'right_shift']]]),

      /// Home Row - Mods
      remap(['a'], [], { setVariables: { MEH: { to: true, to_after_key_up: false } } }),
      remapToStickyModifier('s', 'option', false),
      remapToStickyModifier('d', 'command', false),
      remapToStickyModifier('f', 'control', false),
      // G key: reserved to toggle Mic with HammerSpoon 🔨🥄
      remap(['g'], [['g', superMods]]),

      /// Bottom Row
      remap(['z'], [['up_arrow', ['control']]]),
      remap(['x'], [['tab', ['command', 'shift']]]),
      remap(['c'], [['down_arrow', ['control']]]),
      remap(['v'], [['grave_accent_and_tilde', ['command']]]),
      remap(['b'], [['tab', ['command']]]),
    ],
  },
  {
    description:
      'SUPER keys for Sticky Mods and quick access to the bindings I use most often in right hand',
    manipulators: [
      /// Top Row
      remap(['y'], [['spacebar', ['right_control', 'right_command']]]), // 👾
      // U key: reserved to launch 1p quick access
      remap(['u'], [['u', superMods]]),
      // I key: reserved to launch Alfred
      remap(['i'], [['i', superMods]]),
      // O key: reserved for Alfred's snippets
      remap(['o'], [['o', superMods]]),
      // P key: reserved for Alfred's Universal Access
      remap(['p'], [['p', superMods]]),

      /// Home Row - Mods
      // H key: reserved to switch the sound output
      remap(['h'], [['h', superMods]]),
      remapToStickyModifier('j', 'control', true),
      remapToStickyModifier('k', 'command', true),
      remapToStickyModifier('l', 'option', true),
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
