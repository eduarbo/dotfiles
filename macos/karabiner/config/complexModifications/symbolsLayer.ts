import { remap, ignoreKeebs } from '../../lib';
import type {
  Modifier,
  ModifierOptional,
  ToKeyCodeTuple,
  KeyCode,
  ComplexModifications,
} from '../../lib';

const LAYER = 'SYMBOLS';
const layerMods: Modifier[] = ['right_shift'];
const optionalMods: ModifierOptional[] = ['left_shift', 'right_command', 'right_control'];

const manipulatorOptions = {
  conditions: ignoreKeebs,
};

const keybind = (fromKeyCode: KeyCode, toTuples: ToKeyCodeTuple[]) =>
  remap([fromKeyCode, layerMods, optionalMods], toTuples, {
    manipulatorOptions,
  });

const rules = [
  {
    description: `${LAYER} layer: Left hand - Numpad`,
    manipulators: [
      /// Top Row
      keybind('q', [['home']]), // ⇱
      keybind('w', [['7']]),
      keybind('e', [['8']]),
      keybind('r', [['9']]),
      keybind('t', [['0']]),

      /// Home Row
      keybind('a', [['end']]), // ⇲
      keybind('s', [['4']]),
      keybind('d', [['5']]),
      keybind('f', [['6']]),
      keybind('g', [['equal_sign']]), // =

      /// Bottom Row
      remap(['z', layerMods, ['right_command', 'right_control']], [['e', ['option']]], {
        manipulatorOptions,
      }), // accent
      remap(['z', [...layerMods, 'left_shift'], ['caps_lock']], [['caps_lock']], {
        manipulatorOptions,
      }), // CAPS_LOCK
      keybind('x', [['1']]),
      keybind('c', [['2']]),
      keybind('v', [['3']]),
      keybind('b', [['hyphen']]), // -
    ],
  },
  {
    description: `${LAYER} layer: Right hand - Symbols and arrows`,
    manipulators: [
      /// Top Row
      keybind('y', [['backslash']]), // \
      keybind('u', [['open_bracket']]), // [
      keybind('i', [['close_bracket']]), // ]
      keybind('o', [['slash']]), // /
      keybind('p', [['page_up']]), // ▲

      /// Home Row
      keybind('h', [['left_arrow']]), // ←
      keybind('j', [['down_arrow']]), // ↓
      keybind('k', [['up_arrow']]), // ↑
      keybind('l', [['right_arrow']]), // →
      keybind('semicolon', [['page_down']]), // ▼

      /// Bottom Row
      keybind('n', [['grave_accent_and_tilde']]), // `
      keybind('m', [['quote']]), // '

      // // NOTE Do not shift these, I want them to be available in the same layer as the numpad
      // keybind('comma', [['comma']]),
      // keybind('period', [['period']]),
      keybind('slash', [['delete_or_backspace']]),

      // remap(['comma', layerMods, ['caps_lock']], [['e', ['option']]], {
      //   manipulatorOptions,
      // }), // acento ´
      remap(['comma', ['left_shift', ...layerMods], ['caps_lock']], [['e', ['option']]], {
        manipulatorOptions,
      }), // acento ´

      // remap(['period', layerMods, ['caps_lock']], [['n', ['option']]], {
      //   manipulatorOptions,
      // }), // virgulilla (~)
      remap(['period', ['left_shift', ...layerMods], ['caps_lock']], [['n', ['option']]], {
        manipulatorOptions,
      }), // virgulilla (~)

      // TODO create function to bind keys in this way:
      // bind('comma', layerMods, ['right_command', 'right_control'])
      //   .to([['e', ['option']]])
      //   .condition(ignoreKeebs),
    ],
  },
];

// FIXME: The built-in keyboard on MacBooks experiences ghosting with the keys `C`, `V`, `B`, `N`, `M`, `,`, `.`, and `/` when both Command keys are pressed together. A possible workaround is to remap the modifiers so that the Left CMD + Right CMD combo is reserved for infrequent keybinds.
// Suggestions:
// - Spacebar: Modtap - Space/Shift
// - Left CMD: Modtap - Enter/CMD
// - Right CMD: Modtap - Tab/Symbols (Right Shift)

export const symbolsLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
