const { profile } = require('../utils');
const { corneKeyboard } = require('./devices');

const arrows = require('./mods/arrows');
const emacs = require('./mods/emacs');
const fn = require('./mods/fn');
const launcher = require('./mods/launcher');
const qwerty = require('./mods/qwerty');
const qwertyAlt = require('./mods/qwerty-alt');
const symbols = require('./mods/symbols');
const symbolsAlt = require('./mods/symbols-alt');

const rev2 = [
  arrows,
  emacs,
  fn,
  symbols,
  qwerty,
  launcher,
];

const karabiner = {
  profiles: [
    profile('Clean'),
    profile('Mac & Cheese', [
      arrows,
      emacs,
      fn,
      symbols,
      qwerty,
      launcher,
    ], {
    }),
    profile('Mac & Cheese (Mac kbd OFF if Corne ON)', rev2, {
      devices: [{
        identifiers: corneKeyboard,
        disable_built_in_keyboard_if_exists: true,
        ignore: false,
        manipulate_caps_lock_led: true,
      }],
    }),
    profile('eduarbo v3', [
      arrows,
      emacs,
      fn,
      symbolsAlt,
      qwertyAlt,
      launcher,
    ], {
      selected: true,
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
