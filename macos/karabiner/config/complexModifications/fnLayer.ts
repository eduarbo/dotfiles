import { remap, modTap, remapToStickyModifier } from '../../lib';
import type {
  ComplexModifications,
  KeyCode,
  Modifier,
  ToKeyCodeTuple,
  Manipulator,
  RemapOptions,
} from '../../lib';

const LAYER = 'FN';
const layerMods: Modifier[] = ['fn'];
const toLayerMods: Modifier[] = ['shift', 'option', 'control'];
const optionalMods: Modifier[] = [];

const keybind = (
  keyCode: KeyCode,
  toTuples: ToKeyCodeTuple[],
  options: RemapOptions = {},
): Manipulator => remap([keyCode, layerMods, optionalMods], toTuples, options);

const stickyMod = (fromKeyCode: KeyCode, toStickyModifiers: Modifier[]): Manipulator => {
  const isLeft = toStickyModifiers[0].indexOf('left_') === 0;
  const LEFT_MODS = ['left_option', 'left_command', 'left_control'] as Modifier[];
  const RIGHT_MODS = ['right_option', 'right_command', 'right_control'] as Modifier[];
  const baseModifiers = isLeft ? LEFT_MODS : RIGHT_MODS;

  const setVariables = toStickyModifiers.reduce(
    (acc, mod) => ({
      ...acc,
      [mod.toUpperCase()]: { to: true, to_after_key_up: false },
    }),
    {},
  );

  return remapToStickyModifier([fromKeyCode, layerMods, baseModifiers], toStickyModifiers, {
    setVariables,
  });
};

const rules = [
  {
    description: `${LAYER} layer: Home Row - Sticky Mods`,
    manipulators: [
      /// Left Mods
      stickyMod('a', ['left_option', 'left_command', 'left_control']),
      stickyMod('s', ['left_option']),
      stickyMod('d', ['left_command']),
      stickyMod('f', ['left_control']),
      stickyMod('g', ['left_option', 'left_command', 'left_control', 'left_shift']),

      /// Right Mods
      stickyMod('h', ['right_option', 'right_command', 'right_control', 'left_shift']),
      stickyMod('j', ['right_control']),
      stickyMod('k', ['right_command']),
      stickyMod('l', ['right_option']),
      stickyMod('semicolon', ['right_option', 'right_command', 'right_control']),
    ],
  },
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      // L Command -> Move to Right Desktop
      modTap(
        ['left_command', layerMods, ['shift']],
        [['left_shift']],
        [['left_arrow', ['control']]],
      ),

      // R Command -> Move to Left Desktop
      modTap(
        ['right_command', layerMods, ['shift']],
        [['right_shift']],
        [['right_arrow', ['control']]],
      ),
    ],
  },
  {
    description: `${LAYER} layer: Left hand - Sticky Mods and Window/App Nav`,
    manipulators: [
      /// Top Row - Nav
      // Q key: reserved to switch the sound output
      keybind('q', [['q', toLayerMods]]),
      // Prev
      keybind('w', [['open_bracket', ['command', 'shift']]]),
      // Back
      keybind('e', [['open_bracket', ['command']]]),
      // Forward
      keybind('r', [['close_bracket', ['command']]]),
      // Next
      keybind('t', [['close_bracket', ['command', 'shift']]]),

      /// Home Row - Reserverd for Mods

      /// Bottom Row
      // Z key: reserved to toggle Mic with HammerSpoon 🔨🥄
      keybind('z', [['z', toLayerMods]]),
      // Show all windows of the front app
      keybind('x', [['down_arrow', ['control']]]),
      // Switch back to prev window | go backwards in the application switcher
      keybind('c', [['grave_accent_and_tilde', ['command']]]),
      // Application switcher
      keybind('v', [['tab', ['command']]]),
      // Show all openwindows
      keybind('b', [['up_arrow', ['control']]]),
    ],
  },
  {
    description: `${LAYER} layer: Right hand - Sticky Mods, alfred features, and page nav`,
    manipulators: [
      /// Top Row
      // P key: reserved for Alfred's Universal Access
      keybind('y', [['y', toLayerMods]]),
      // U key: reserved to launch 1p quick access
      keybind('u', [['u', toLayerMods]]),
      // I key: reserved to launch Alfred
      keybind('i', [['i', toLayerMods]]),
      // O key: reserved for Alfred's snippets
      keybind('o', [['o', toLayerMods]]),
      // P key: avalilable
      keybind('p', [['p', toLayerMods]]),

      /// Home Row - Reserverd for Mods

      /// Bottom Row - Reserved to manage windows with HammerSpoon 🔨🥄
      keybind('n', [['n', toLayerMods]]),
      keybind('m', [['m', toLayerMods]]),
      keybind('comma', [['comma', toLayerMods]]),
      keybind('period', [['period', toLayerMods]]),
      // TODO Show shortcuts cheatsheet
      keybind('slash', [['slash', toLayerMods]]),
    ],
  },
];

export const fnLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
