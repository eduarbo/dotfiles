import { profile } from '../lib';
import type { KarabinerConfig } from '../lib';
import { supr, emacs, symbols, meh, base } from './complexModifications';

// NOTE Do NOT move them! order matters: The more specific the binding, the sooner it should be declared
const defaultProfile = [
  emacs,
  meh,
  supr,
  symbols,
  base,
];

const profiles = [
  profile('Empty'),
  profile('Default', defaultProfile, {
    selected: true,
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
