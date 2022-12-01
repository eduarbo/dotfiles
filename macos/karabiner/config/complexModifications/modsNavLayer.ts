import { remap, modTap, remapToStickyModifier, BASE_MODIFIERS } from '../../lib';
import type {
  ComplexModifications,
  KeyCode,
  Modifier,
  ToKeyCodeTuple,
  Manipulator,
  RemapOptions,
  BaseModifier,
} from '../../lib';

const LAYER = 'MODS_NAV';
const layerMods: Modifier[] = [];
const toLayerMods: Modifier[] = ['shift', 'option', 'control'];
const optionalMods: Modifier[] = [];

const ifLayer = {
  type: 'variable_if',
  name: LAYER,
  value: true,
};

const keybind = (
  keyCode: KeyCode,
  toTuples: ToKeyCodeTuple[],
  options: RemapOptions = {},
): Manipulator =>
  remap([keyCode, layerMods, optionalMods], toTuples, {
    manipulatorOptions: {
      conditions: [ifLayer],
    },
    ...options,
  });

const stickyMod = (
  fromKeyCode: KeyCode,
  toStickyModifiers: BaseModifier[],
  isRightSide: boolean,
): Manipulator => {
  const remainingBaseMods = BASE_MODIFIERS.filter((mod) => toStickyModifiers.indexOf(mod) < 0);

  const getSideMods = (mods: BaseModifier[]) =>
    mods.map((mod) => `${isRightSide ? 'right' : 'left'}_${mod}` as Modifier);
  const toModifiers: Modifier[] = getSideMods(toStickyModifiers);
  const setVariables = toModifiers.reduce(
    (acc, mod) => ({
      ...acc,
      [mod.toUpperCase()]: { to: true, to_after_key_up: false },
    }),
    {},
  );

  return remapToStickyModifier(
    [fromKeyCode, null, getSideMods(remainingBaseMods)],
    toModifiers,
    {
      setVariables,
      manipulatorOptions: {
        conditions: [ifLayer],
      },
    },
  );
};

const rules = [
  {
    description: `${LAYER} layer: Home Row - Sticky Mods`,
    manipulators: [
      /// Left Mods
      stickyMod('a', ['option'], false),
      stickyMod('s', ['shift'], false),
      stickyMod('d', ['command'], false),
      stickyMod('f', ['control'], false),
      stickyMod('g', ['option', 'command', 'control'], false),

      /// Right Mods
      stickyMod('h', ['option', 'command', 'control'], true),
      stickyMod('j', ['control'], true),
      stickyMod('k', ['command'], true),
      stickyMod('l', ['shift'], true),
      stickyMod('semicolon', ['option'], true),
    ],
  },
  {
    description: `${LAYER} layer: Thumbs cluster`,
    manipulators: [
      // L Command -> Move to Right Desktop
      modTap(['left_command', null, ['shift']], [['left_shift']], [['left_arrow', ['control']]], {
        manipulatorOptions: {
          conditions: [ifLayer],
        },
      }),

      // R Command -> Move to Left Desktop
      modTap(
        ['right_command', null, ['shift']],
        [['right_shift']],
        [['right_arrow', ['control']]],
        {
          manipulatorOptions: {
            conditions: [ifLayer],
          },
        },
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

export const modsNavLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
