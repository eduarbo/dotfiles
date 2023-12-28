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
      // Toggle script execution
      keybind('w', [['backslash', ['command']]]),
      // Toggle DevTools
      keybind('e', [['i', ['option', 'command']]]),
      // Inspect element
      keybind('r', [['c', ['shift', 'command']]]),
      // NOTE T - Toggle Mic (With Hammerspoon)

      /// Home Row - Nav
      // Prev
      keybind('s', [['open_bracket', ['command', 'shift']]]),
      // Back
      keybind('d', [['open_bracket', ['command']]]),
      // Forward
      keybind('f', [['close_bracket', ['command']]]),
      // Next
      keybind('g', [['close_bracket', ['command', 'shift']]]),

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
  {
    description: `${LAYER} layer: Right hand - Window manager and shortcuts`,
    manipulators: [
      /// Top Row
      // NOTE Y - Switch Audio output (With Hammerspoon)
      // NOTE U - 1P Quick Access
      // NOTE I - Autofill 1P
      // NOTE O - Alfred's snippets
      // NOTE P - Alfred's Universal Access

      /// Home Row (HammerSpoon bindings)
      // NOTE H - moves window to the left half of the screen
      // NOTE J - moves window to the left third of the screen
      // NOTE K - moves window to the right third of the screen
      // NOTE L - moves window to the right half of the screen
      // NOTE ; - Lock screen

      /// Bottom Row
      keybind('n', [['a', ['shift', 'command']]]),
      // NOTE M - MacGPT
      // NOTE , - not bound
      // NOTE . - not bound
      // NOTE ⌫ - not bound
    ],
  },
];

export const superLayer: ComplexModifications = { title: `${LAYER} layer`, rules };
