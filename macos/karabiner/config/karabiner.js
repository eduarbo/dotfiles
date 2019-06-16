const { profile } = require('../utils');
const { corneKeyboard } = require('./devices');

const arrows = require('./mods/arrows');
const emacs = require('./mods/emacs');
const fn = require('./mods/fn');
const launcher = require('./mods/launcher');
const qwerty = require('./mods/qwerty');
const symbols = require('./mods/symbols');

const karabiner = {
  profiles: [
    profile('Clean'),
    profile('Rev 1', [
      arrows,
      emacs,
      fn,
      symbols,
      qwerty,
      launcher,
    ], {
      selected: true,
      devices: [{
        identifiers: corneKeyboard,
        disable_built_in_keyboard_if_exists: false,
        ignore: false,
        manipulate_caps_lock_led: true,
      }],
    }),
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
  launcher,
  qwerty,
  symbols,
};

module.exports = {
  karabiner,
  complexModifications,
};
