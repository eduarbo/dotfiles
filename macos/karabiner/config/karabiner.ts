import { profile, karabinerToJson } from '../lib';
import { hyper, emacs, launcher, symbols, main } from './complexModifications';

// NOTE Do NOT move them! order matters: The more specific the binding, the sooner it should be declared
const defaultProfile = [
  emacs,
  hyper,
  symbols,
  launcher,
  main,
  // TODO
  // fn,
];

const profiles = [
  profile('Empty'),
  profile('Default', defaultProfile, {
    selected: true,
  }),
];

const globalConfig = {
  check_for_updates_on_startup: true,
  show_in_menu_bar: true,
  show_profile_name_in_menu_bar: false,
};

karabinerToJson(profiles, globalConfig);
