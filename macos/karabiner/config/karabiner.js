const { profile } = require('../utils');
const { corneKeyboard } = require('./devices');

const launcher = require('./mods/launcher');
const arrows = require('./mods/arrows');

const karabiner = {
  profiles: [
    profile('Clean'),
    profile('Rev 1', [arrows, launcher], {
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
};

module.exports = {
  karabiner,
  complexModifications,
};
