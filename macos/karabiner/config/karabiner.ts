import { profile, CRKBD, BEEKEEB } from '../lib/index.js';
import type { KarabinerConfig } from '../lib/index.js';
import {
  emacsKeybindings,
  symbolsLayer,
  superLayer,
  fnLayer,
  baseLayer,
  civLayer,
} from './complexModifications/index.js';

// Order matters: first match wins. More specific bindings go first.
const defaultProfile = [
  emacsKeybindings,
  superLayer,
  fnLayer,
  symbolsLayer,
  baseLayer,
  civLayer,
];

const profiles = [
  profile('Empty'),
  profile('Default', defaultProfile),
  profile('Default (disable built-in kbd when CRKBD is connected)', defaultProfile, {
    selected: true,
    devices: [
      {
        identifiers: CRKBD,
        disable_built_in_keyboard_if_exists: true,
        ignore: false,
        manipulate_caps_lock_led: true,
      },
      {
        identifiers: BEEKEEB,
        disable_built_in_keyboard_if_exists: true,
        ignore: false,
        manipulate_caps_lock_led: true,
      },
    ],
  }),
];

const globalSettings = {
  check_for_updates_on_startup: true,
  show_in_menu_bar: true,
  show_profile_name_in_menu_bar: false,
  indicate_sticky_modifier_keys_state: true,
};

export const karabiner: KarabinerConfig = {
  profiles,
  global: globalSettings,
};

export * as complexModifications from './complexModifications/index.js';
