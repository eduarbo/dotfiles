import { remap } from '../../lib/index.js';
import type { KeyCode, Modifier, ToKeyCodeTuple, ComplexModifications } from '../../lib/index.js';

const LAYER = 'SUPER';
const layerMods: Modifier[] = ['option', 'command', 'control'];
const optionalMods: Modifier[] = [];

const keybind = (keyCode: KeyCode, toTuples: ToKeyCodeTuple[]) =>
  remap([keyCode, layerMods, optionalMods], toTuples);

const rules = [
  {
    description: `${LAYER} layer: Left hand - dev tools, screenshots and tab/history navigation`,
    manipulators: [
      // Top Row - Dev Tools
      keybind('e', [['i', ['option', 'command']]]),
      keybind('r', [['c', ['shift', 'command']]]),
      keybind('t', [['backslash', ['command']]]),

      // Home Row - Nav
      keybind('s', [['tab', ['control', 'shift']]]),
      keybind('d', [['open_bracket', ['command']]]),
      keybind('f', [['close_bracket', ['command']]]),
      keybind('g', [['tab', ['control']]]),

      // Bottom Row
      keybind('x', [['down_arrow', ['control']]]),
      keybind('c', [['grave_accent_and_tilde', ['command']]]),
      keybind('v', [['tab', ['command']]]),
      keybind('b', [['up_arrow', ['control']]]),

      // Pinkies - Screenshots
      keybind('q', [['3', ['shift', 'command']]]),
      keybind('a', [['4', ['shift', 'command']]]),
      keybind('z', [['5', ['shift', 'command']]]),
    ],
  },
];

export const superLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
