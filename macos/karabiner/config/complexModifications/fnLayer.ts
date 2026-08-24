import { remap, ignoreKeebs } from '../../lib/index.js';
import type {
  ComplexModifications,
  KeyCode,
  Modifier,
  ToKeyCodeTuple,
  Manipulator,
  RemapOptions,
  ManipulatorOptions,
} from '../../lib/index.js';

const LAYER = 'FN';
const layerMods: Modifier[] = ['left_command', 'left_control', 'left_option', 'right_control'];
const optionalMods: Modifier[] = [];

const manipulatorOptions: ManipulatorOptions = {
  conditions: ignoreKeebs,
};

const keybind = (
  keyCode: KeyCode,
  toTuples: ToKeyCodeTuple[],
  options: RemapOptions = {},
): Manipulator =>
  remap([keyCode, layerMods, optionalMods], toTuples, {
    manipulatorOptions,
    toOptions: { lazy: true },
    ...options,
  });

const rules = [
  {
    description: `${LAYER} layer: Left hand - Function keys`,
    manipulators: [
      keybind('q', [['f12']]),
      keybind('w', [['f7']]),
      keybind('e', [['f8']]),
      keybind('r', [['f9']]),
      keybind('t', [['print_screen']]),

      keybind('a', [['f11']]),
      keybind('s', [['f4']]),
      keybind('d', [['f5']]),
      keybind('f', [['f6']]),
      keybind('g', [['locking_scroll_lock']]),

      keybind('z', [['f10']]),
      keybind('x', [['f1']]),
      keybind('c', [['f2']]),
      keybind('v', [['f3']]),
      keybind('b', [['pause']]),
    ],
  },
  {
    description: `${LAYER} layer: Right hand - Media and Brightness`,
    manipulators: [
      keybind('y', [['volume_increment']]),
      keybind('u', [['rewind']]),
      keybind('i', [['play_or_pause']]),
      keybind('o', [['fastforward']]),
      keybind('p', [['print_screen']]),

      keybind('h', [['volume_decrement']]),
      keybind('j', [['volume_decrement']]),
      keybind('k', [['mute']]),
      keybind('l', [['volume_increment']]),
      keybind('semicolon', [['locking_scroll_lock']]),

      keybind('n', [['mute']]),
      keybind('m', [['display_brightness_decrement']]),
      keybind('comma', [['display_brightness_increment']]),
      keybind('period', [['mission_control']]),
      keybind('slash', [['pause']]),
    ],
  },
];

export const fnLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
