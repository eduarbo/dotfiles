import { remap, ignoreKeebs } from '../../lib';
import type {
  ComplexModifications,
  KeyCode,
  Modifier,
  ToKeyCodeTuple,
  Manipulator,
  RemapOptions,
  ManipulatorOptions,
} from '../../lib';

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
    toOptions: {
      lazy: true,
    },
    ...options,
  });

const rules = [
  {
    description: `${LAYER} layer: Left hand - Sticky Mods and Window/App Nav`,
    manipulators: [
      /// Top Row - Nav
      keybind('q', [['volume_increment']]),
      keybind('w', [['rewind']]),
      keybind('e', [['play_or_pause']]),
      keybind('r', [['fastforward']]),
      keybind('t', [['print_screen']]),

      /// Home Row - Reserverd for Mods
      keybind('a', [['volume_decrement']]),
      keybind('s', [['volume_decrement']]),
      keybind('d', [['mute']]),
      keybind('f', [['volume_increment']]),
      keybind('g', [['locking_scroll_lock']]),

      /// Bottom Row
      keybind('z', [['mute']]),
      keybind('x', [['display_brightness_decrement']]),
      keybind('c', [['display_brightness_increment']]),
      keybind('v', [['mission_control']]),
      keybind('b', [['pause']]),
    ],
  },
  {
    description: `${LAYER} layer: Right hand - Sticky Mods, alfred features, and page nav`,
    manipulators: [
      /// Top Row - Nav
      keybind('y', [['volume_increment']]),
      keybind('u', [['rewind']]),
      keybind('i', [['play_or_pause']]),
      keybind('o', [['fastforward']]),
      keybind('p', [['print_screen']]),

      /// Home Row - Reserverd for Mods
      keybind('h', [['volume_decrement']]),
      keybind('j', [['volume_decrement']]),
      keybind('k', [['mute']]),
      keybind('l', [['volume_increment']]),
      keybind('semicolon', [['locking_scroll_lock']]),

      /// Bottom Row
      keybind('n', [['mute']]),
      keybind('m', [['display_brightness_decrement']]),
      keybind('comma', [['display_brightness_increment']]),
      keybind('period', [['mission_control']]),
      keybind('slash', [['pause']]),
    ],
  },
];

export const fnLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
