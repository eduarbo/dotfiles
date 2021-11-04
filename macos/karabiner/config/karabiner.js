const { profile } = require('../utils');
const { corneKeyboard } = require('./devices');

const arrows = require('./mods/arrows');
const emacs = require('./mods/emacs');
const fn = require('./mods/fn');
const oldQwerty = require('./mods/old-qwerty');
const qwerty = require('./mods/qwerty');
const oldSymbols = require('./mods/old-symbols');
const symbols = require('./mods/symbols');

const rev3 = [
  arrows,
  emacs,
  fn,
  oldSymbols,
  oldQwerty,
];

const rev4 = [
  arrows,
  emacs,
  fn,
  symbols,
  qwerty,
];

const karabiner = {
  profiles: [
    profile('Clean'),
    profile('eduarbo v3', rev3, {
      selected: true,
    }),
    profile('eduarbo v3 (Mac kbd OFF if Corne ON)', rev3, {
      devices: [{
        identifiers: corneKeyboard,
        disable_built_in_keyboard_if_exists: true,
        ignore: false,
        manipulate_caps_lock_led: true,
      }],
    }),
    profile('eduarbo v4', rev4, {}),
  ],
  global: {
    check_for_updates_on_startup: true,
    show_in_menu_bar: true,
    show_profile_name_in_menu_bar: false,
  },
};

const complexModifications = {
  arrows,
  emacs,
  fn,
  symbols,
  qwerty,
};

module.exports = {
  karabiner,
  complexModifications,
};
