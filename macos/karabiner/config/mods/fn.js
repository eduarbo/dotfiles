const { remap } = require('../../utils');

const mandatoryMods = ['left_shift', 'right_shift'];
const optionalMods = [];
const remapToLayer = (fromKey, toKey) => remap([fromKey, mandatoryMods, optionalMods], toKey);

module.exports = {
  title: 'Fn layer',
  rules: [
    {
      description: 'Function keys, movement, brightness, volumen and media controls',
      manipulators: [
        // Volume controls
        remapToLayer('q', [['mute']]),
        remapToLayer('w', [['volume_decrement']]),
        remapToLayer('r', [['volume_increment']]),

        // Brightness controls %>
        remapToLayer('a', [['display_brightness_decrement']]),
        remapToLayer('g', [['display_brightness_increment']]),

        // Music player
        remapToLayer('x', [['rewind']]),
        remapToLayer('c', [['play_or_pause']]),
        remapToLayer('v', [['fastforward']]),

        // Movement left hand
        remapToLayer('e', [['up_arrow']]),
        remapToLayer('s', [['left_arrow']]),
        remapToLayer('d', [['down_arrow']]),
        remapToLayer('f', [['right_arrow']]),

        // HJKL Movement
        remapToLayer('h', [['left_arrow']]),
        remapToLayer('j', [['down_arrow']]),
        remapToLayer('k', [['up_arrow']]),
        remapToLayer('l', [['right_arrow']]),

        // Page navigation
        remapToLayer('i', [['page_up']]),
        remapToLayer('u', [['page_down']]),
        // FIXME
        // remapToLayer('comma', [['home']]),
        // remapToLayer('m', [['end']]),
      ],
    },
  ],
};
