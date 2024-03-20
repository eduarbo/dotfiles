import { profile, CRKBD, BEEKEEB, type KarabinerConfig } from '../lib';
import {
  emacsKeybindings,
  symbolsLayer,
  superLayer,
  fnLayer,
  baseLayer,
  // gamingLayer,
} from './complexModifications';

// NOTE Do NOT move them! order matters: The more specific the binding, the sooner it should be declared
const defaultProfile = [
  emacsKeybindings,
  superLayer,
  fnLayer,
  symbolsLayer,
  baseLayer,
  // gamingLayer,
];

const profiles = [
  profile('Empty'),
  profile('Default', defaultProfile, {
    // selected: true,
  }),
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

export * as complexModifications from './complexModifications';
