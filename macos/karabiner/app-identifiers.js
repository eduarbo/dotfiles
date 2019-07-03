const browser = [
  '^org\\.mozilla\\.firefox$',
  '^com\\.google\\.Chrome$',
  '^com\\.apple\\.Safari$',
];

const emacs = [
  '^org\\.gnu\\.Emacs$',
  '^org\\.gnu\\.AquamacsEmacs$',
  '^org\\.gnu\\.Aquamacs$',
  '^org\\.pqrs\\.unknownapp.conkeror$',
];

const remoteDesktop = [
  '^com\\.microsoft\\.rdc$',
  '^com\\.microsoft\\.rdc\\.mac$',
  '^com\\.microsoft\\.rdc\\.osx\\.beta$',
  '^net\\.sf\\.cord$',
  '^com\\.thinomenon\\.RemoteDesktopConnection$',
  '^com\\.itap-mobile\\.qmote$',
  '^com\\.nulana\\.remotixmac$',
  '^com\\.p5sys\\.jump\\.mac\\.viewer$',
  '^com\\.p5sys\\.jump\\.mac\\.viewer\\.web$',
  '^com\\.vmware\\.horizon$',
  '^com\\.2X\\.Client\\.Mac$',
];

const terminal = [
  '^com\\.apple\\.Terminal$',
  '^com\\.googlecode\\.iterm2$',
  '^co\\.zeit\\.hyperterm$',
  '^co\\.zeit\\.hyper$',
  '^net.kovidgoyal.kitty$',
];

const vi = [
  '^org\\.vim\\.', // prefix
];

const virtualMachine = [
  '^com\\.vmware\\.fusion$',
  '^com\\.vmware\\.horizon$',
  '^com\\.vmware\\.view$',
  '^com\\.parallels\\.desktop$',
  '^com\\.parallels\\.vm$',
  '^com\\.parallels\\.desktop\\.console$',
  '^org\\.virtualbox\\.app\\.VirtualBoxVM$',
  '^com\\.vmware\\.proxyApp\\.', // prefix
  '^com\\.parallels\\.winapp\\.', // prefix
];

const x11 = [
  '^org\\.x\\.X11$',
  '^com\\.apple\\.x11$',
  '^org\\.macosforge\\.xquartz\\.X11$',
  '^org\\.macports\\.X11$',
];

module.exports = {
  terminal,
  emacs,
  remoteDesktop,
  vi,
  virtualMachine,
  browser,
  emacsKeyBindingsException: [].concat(
    emacs,
    remoteDesktop,
    terminal,
    vi,
    virtualMachine,
    x11,
  ),
};
