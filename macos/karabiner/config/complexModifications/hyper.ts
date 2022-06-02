import { remap } from '../../lib';
import type { KeyCode, Modifier, ToKeyCodeTuple, ComplexModifications } from '../../lib';

const mandatoryMods = ['left_shift', 'right_shift'] as Modifier[];

const keybind = (keyCode: KeyCode, toTuples: ToKeyCodeTuple[], optionalMods: Modifier[] = []) =>
  remap([keyCode, mandatoryMods, optionalMods], toTuples, {
    conditions: [
      {
        type: 'device_unless',
        identifiers: [{
          vendor_id: 18003
        }]
      },
    ],
  });

const rules = [
  {
    description: 'Function keys for left hand - movement, brightness, volumen and media controls',
    manipulators: [
      // Volume controls
      keybind('e', [['mute']]),
      keybind('w', [['volume_decrement']]),
      keybind('r', [['volume_increment']]),

      // Toggle audio output
      keybind('q', [['f14']]),
      // Toggle mic
      keybind('t', [['f13']]),

      // TODO have a copy of all the modifiers in the home row

      // Brightness controls
      keybind('z', [['display_brightness_decrement']]),
      // FIXME all of the following bindings are broken only on the macbook keyboard and I have no idea
      keybind('b', [['display_brightness_increment']]),

      // Music player
      keybind('x', [['rewind']]),
      keybind('c', [['play_or_pause']]),
      keybind('v', [['fastforward']]),
    ],
  },
  {
    description: 'Function keys for right hand - movement, brightness, volumen and media controls',
    manipulators: [
      // Volume controls
      keybind('i', [['mute']]),
      keybind('u', [['volume_decrement']]),
      keybind('o', [['volume_increment']]),

      // Toggle audio output
      keybind('p', [['f14']]),
      // Toggle mic
      keybind('y', [['f13']]),

      // TODO have a copy of all the modifiers in the home row

      // FIXME all of the following bindings are broken only on the macbook keyboard and I have no idea

      // Brightness controls
      keybind('n', [['display_brightness_decrement']]),
      keybind('slash', [['display_brightness_increment']]),

      // Music player
      keybind('m', [['rewind']]),
      keybind('comma', [['play_or_pause']]),
      keybind('period', [['fastforward']]),
    ],
  },
];

export const hyper: ComplexModifications = { title: 'Hyper layer', rules };
