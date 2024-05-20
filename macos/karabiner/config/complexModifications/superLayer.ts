import { remap } from '../../lib';
import type { KeyCode, Modifier, ToKeyCodeTuple, ComplexModifications } from '../../lib';

const LAYER = 'SUPER';
const layerMods: Modifier[] = ['option', 'command', 'control'];
const optionalMods: Modifier[] = [];

const keybind = (keyCode: KeyCode, toTuples: ToKeyCodeTuple[]) =>
  remap([keyCode, layerMods, optionalMods], toTuples);

const rules = [
  {
    description: `${LAYER} layer: Left hand - dev tools, screenshots and tab/history navigation`,
    manipulators: [
      /// Top Row - Dev Tools
      // NOTE W - Toggle Mic (With Hammerspoon)
      // Toggle DevTools
      keybind('e', [['i', ['option', 'command']]]),
      // Inspect element
      keybind('r', [['c', ['shift', 'command']]]),
      // Toggle script execution
      keybind('t', [['backslash', ['command']]]),

      /// Home Row - Nav
      // Prev
      keybind('s', [['tab', ['command', 'shift']]]),
      // Back
      keybind('d', [['open_bracket', ['command']]]),
      // Forward
      keybind('f', [['close_bracket', ['command']]]),
      // Next
      keybind('g', [['tab', ['command', 'shift']]]),

      /// Bottom Row
      // Show all windows of the front app
      keybind('x', [['down_arrow', ['control']]]),
      // Switch back to prev window | go backwards in the application switcher
      keybind('c', [['grave_accent_and_tilde', ['command']]]),
      // Application switcher
      keybind('v', [['tab', ['command']]]),
      // Show all openwindows
      keybind('b', [['up_arrow', ['control']]]),

      /// Pinkies - Screenshots
      // screenshot
      keybind('q', [['3', ['shift', 'command']]]),
      // capture a portion of the screen
      keybind('a', [['4', ['shift', 'command']]]),
      // record screen
      keybind('z', [['5', ['shift', 'command']]]),
    ],
  },
];

export const superLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
