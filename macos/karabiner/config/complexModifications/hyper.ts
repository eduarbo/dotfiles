import { complexModifications, remap } from '../../lib';
import type { KeyCode, Modifier, ToKeyCodeTuple } from '../../lib';

const mandatoryMods = ['left_shift', 'right_shift'] as Modifier[];

const keybind = (keyCode: KeyCode, toTuples: ToKeyCodeTuple[], optionalMods: Modifier[] = []) =>
  remap([keyCode, mandatoryMods, optionalMods], toTuples);

export const hyper = complexModifications('Hyper layer', [
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

      // Brightness controls
      // TODO remap to a/g
      keybind('z', [['display_brightness_decrement']]),
      // FIXME This is broken on the built-in macOS keyboard
      keybind('b', [['display_brightness_increment']]),

      // Music player
      // TODO remap to s/d/f on tap and modifiers on hold
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

      // FIXME The following bindings are broken on the built-in macOS keyboard

      // Brightness controls
      // TODO remap to h/semicolon
      keybind('n', [['display_brightness_decrement']]),
      keybind('slash', [['display_brightness_increment']]),

      // Music player
      // TODO remap to j/k/l on tap and modifiers on hold
      keybind('m', [['rewind']]),
      keybind('comma', [['play_or_pause']]),
      keybind('period', [['fastforward']]),
    ],
  },
]);
