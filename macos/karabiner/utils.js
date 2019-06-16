const from = (key, mandatory, optional = ['any']) => ({
  key_code: key,
  modifiers: {
    mandatory,
    optional,
  },
});

const to = keys => keys.map(([key, modifiers]) => ({
  key_code: key,
  modifiers,
}));

const manipulator = options => ({
  type: 'basic',
  ...options,
});

// `modTap` acts like a modifier when held, and a regular keycode when tapped. In
// other words, you can have a key that sends Escape when you tap it, but
// functions as a Control or Shift key when you hold it down.
const keyToString = (...args) => args.map(([key, modifiers = []]) => [
  modifiers.length && `${modifiers.join('+')}+`,
  key,
].filter(Boolean).join('')).join(' | ');

const remap = (fromKey, toKey, options) => ({
  description: `${keyToString(fromKey)} to ${keyToString(...toKey)}`,
  type: 'basic',
  from: from(...fromKey),
  to: to(toKey),
  ...options,
});

const modTap = (fromKey, toKey, toKeyOnTap) => ({
  ...remap(fromKey, toKey),
  description: `${keyToString(fromKey)} to ${keyToString(...toKey)}, send ${keyToString(...toKeyOnTap)} on tap`,
  to_if_alone: to(toKeyOnTap),
});

const getRules = mods => mods.reduce((rules, mod) => rules.concat(mod.rules), []);

const profile = (name, mods = [], overrides = {}) => ({
  name,
  devices: [],
  virtual_hid_keyboard: {
    caps_lock_delay_milliseconds: 0,
    country_code: 0,
    keyboard_type: '',
  },
  ...overrides,
  complex_modifications: {
    parameters: {
      'basic.simultaneous_threshold_milliseconds': 50,
      'basic.to_delayed_action_delay_milliseconds': 500,
      'basic.to_if_alone_timeout_milliseconds': 1000,
      'basic.to_if_held_down_threshold_milliseconds': 500,
    },
    rules: getRules(mods),
    ...overrides.complex_modifications,
  },
});

module.exports = {
  from,
  to,
  manipulator,
  remap,
  modTap,
  profile,
};
