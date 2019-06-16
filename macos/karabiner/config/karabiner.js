const { profile } = require('../utils');
const { corneKeyboard } = require('./devices');

const launcher = require('./mods/launcher');
const arrows = require('./mods/arrows');
const qwerty = require('./mods/qwerty');
const symbols = require('./mods/symbols');

const karabiner = {
  profiles: [
    profile('Clean'),
    profile('Rev 1', [arrows, launcher, symbols, qwerty], {
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
  launcher,
  arrows,
  qwerty,
  symbols,
};

module.exports = {
  karabiner,
  complexModifications,
};
