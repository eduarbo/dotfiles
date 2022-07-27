import * as lib from '../../lib';
import { remap } from '../../lib';
import type {
  ComplexModifications,
  KeyCode,
  Modifier,
  ToKeyCodeTuple,
  Manipulator,
  VariableCondition,
} from '../../lib';

const remapToStickyModifier = (
  fromKeyCode: KeyCode,
  toModifier: 'command' | 'option' | 'control' | 'shift',
  isRightSide: boolean,
): Manipulator => {
  const toKeyCode: KeyCode = `${isRightSide ? 'right' : 'left'}_${toModifier}`;

  const modification = lib.remapToStickyModifier([fromKeyCode, null, ['any']], [toKeyCode], {
    setVariables: [[toKeyCode, true]],
    manipulatorOptions: {
      conditions: [
        {
          type: 'variable_if',
          name: 'SUPER',
          value: true,
        },
        ...['option', 'control', 'command'].map(
          (mod): VariableCondition => ({
            type: 'variable_unless',
            name: `${isRightSide ? 'left' : 'right'}_${mod}`,
            value: true,
          }),
        ),
      ],
    },
  });

  return {
    ...modification,
    to_after_key_up: [
      {
        set_variable: {
          name: toKeyCode,
          value: false,
        },
      },
    ],
  };
};

const keybind = (keyCode: KeyCode, toTuples: ToKeyCodeTuple[], optionalMods: Modifier[] = []) =>
  remap([keyCode, null, optionalMods], toTuples, {
    manipulatorOptions: {
      conditions: [
        {
          type: 'variable_if',
          name: 'SUPER',
          value: true,
        },
      ],
    },
  });

const superMods: Modifier[] = ['option', 'command', 'control'];

const rules = [
  {
    description:
      'SUPER keys for Sticky Mods and quick access to the bindings I use most often in left hand',
    manipulators: [
      /// Top Row - Nav
      keybind('q', [['a', ['right_command', 'right_shift']]]),
      keybind('w', [['open_bracket', ['right_command', 'right_shift']]]),
      keybind('e', [['open_bracket', ['right_command']]]),
      keybind('r', [['close_bracket', ['right_command']]]),
      keybind('t', [['close_bracket', ['right_command', 'right_shift']]]),

      /// Home Row - Mods
      // A key: available
      keybind('a', [['a', superMods]]),
      remapToStickyModifier('s', 'option', false),
      remapToStickyModifier('d', 'command', false),
      remapToStickyModifier('f', 'control', false),
      // G key: reserved to toggle Mic with HammerSpoon 🔨🥄
      keybind('g', [['g', superMods]]),

      /// Bottom Row
      keybind('z', [['up_arrow', ['control']]]),
      keybind('x', [['tab', ['command', 'shift']]]),
      keybind('c', [['down_arrow', ['control']]]),
      keybind('v', [['grave_accent_and_tilde', ['command']]]),
      keybind('b', [['tab', ['command']]]),
    ],
  },
  {
    description:
      'SUPER keys for Sticky Mods and quick access to the bindings I use most often in right hand',
    manipulators: [
      /// Top Row
      keybind('y', [['spacebar', ['right_control', 'right_command']]]), // 👾
      // U key: reserved to launch 1p quick access
      keybind('u', [['u', superMods]]),
      // I key: reserved to launch Alfred
      keybind('i', [['i', superMods]]),
      // O key: reserved for Alfred's snippets
      keybind('o', [['o', superMods]]),
      // P key: reserved for Alfred's Universal Access
      keybind('p', [['p', superMods]]),

      /// Home Row - Mods
      // H key: reserved to switch the sound output
      keybind('h', [['h', superMods]]),
      remapToStickyModifier('j', 'control', true),
      remapToStickyModifier('k', 'command', true),
      remapToStickyModifier('l', 'option', true),
      // ; key: available
      keybind('semicolon', [['semicolon', superMods]]),

      /// Bottom Row
      keybind('n', [['home']]),
      keybind('m', [['page_down']]),
      keybind('comma', [['page_up']]),
      keybind('period', [['end']]),
      // / key: available
      keybind('slash', [['slash', superMods]]),

      // SYMBOLS -> HYPER
      keybind('right_command', [['right_shift', superMods]]),
      // SHIFT -> HYPER
      keybind('left_command', [['right_shift', superMods]]),
    ],
  },
];

export const supr: ComplexModifications = { title: 'Super layer', rules };
