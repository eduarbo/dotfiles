module.exports = {
  extends: 'airbnb-base',
  rules: {
    'no-use-before-define': ['error', { functions: false }],
    'no-multi-spaces': ['error', { ignoreEOLComments: true }]
  },
  globals: {
    settings: true,
    Hints: true,
    mapkey: true,
    vmapkey: true,
    imapkey: true,
    map: true,
    unmap: true,
    iunmap: true,
    tabOpenLink: true,
    Front: true,
    runtime: true,
    RUNTIME: true,
    Normal: true,
    removeSearchAliasX: true,
  },
  env: {
    browser: true,
  }
};
