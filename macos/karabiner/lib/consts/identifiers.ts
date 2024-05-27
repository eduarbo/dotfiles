export const BROWSER = [
  '^org\\.mozilla\\.firefox$',
  '^com\\.google\\.Chrome$',
  '^com\\.apple\\.Safari$',
];

export const EMACS = [
  '^org\\.gnu\\.Emacs$',
  '^org\\.gnu\\.AquamacsEmacs$',
  '^org\\.gnu\\.Aquamacs$',
  '^org\\.pqrs\\.unknownapp.conkeror$',
];

export const VSCODE = ['^com.microsoft.VSCode$'];

export const REMOTE_DESKTOP = [
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

export const TERMINAL = [
  '^com\\.apple\\.Terminal$',
  '^com\\.googlecode\\.iterm2$',
  '^co\\.zeit\\.hyperterm$',
  '^co\\.zeit\\.hyper$',
  '^net.kovidgoyal.kitty$',
];

export const VI = [
  '^org\\.vim\\.', // prefix
];

export const VIRTUAL_MACHINE = [
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

export const X11 = [
  '^org\\.x\\.X11$',
  '^com\\.apple\\.x11$',
  '^org\\.macosforge\\.xquartz\\.X11$',
  '^org\\.macports\\.X11$',
];

export const CIV5 = '^com.aspyr.civ5xp.steam$';

export const GAMES = ['^com\\.factorio$', CIV5];

export const EMACS_KEY_BINDINGS_EXCEPTION = [
  ...EMACS,
  ...VSCODE,
  ...REMOTE_DESKTOP,
  ...TERMINAL,
  ...VI,
  ...VIRTUAL_MACHINE,
  ...X11,
];
