export const browser = [
  '^org\\.mozilla\\.firefox$',
  '^com\\.google\\.Chrome$',
  '^com\\.apple\\.Safari$',
];

export const emacs = [
  '^org\\.gnu\\.Emacs$',
  '^org\\.gnu\\.AquamacsEmacs$',
  '^org\\.gnu\\.Aquamacs$',
  '^org\\.pqrs\\.unknownapp.conkeror$',
];

export const vscode = [
  '^com.microsoft.VSCode$',
];

export const remoteDesktop = [
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

export const terminal = [
  '^com\\.apple\\.Terminal$',
  '^com\\.googlecode\\.iterm2$',
  '^co\\.zeit\\.hyperterm$',
  '^co\\.zeit\\.hyper$',
  '^net.kovidgoyal.kitty$',
];

export const vi = [
  '^org\\.vim\\.', // prefix
];

export const virtualMachine = [
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

export const x11 = [
  '^org\\.x\\.X11$',
  '^com\\.apple\\.x11$',
  '^org\\.macosforge\\.xquartz\\.X11$',
  '^org\\.macports\\.X11$',
];

export const games = [
  '^com\\.factorio$',
];

export const EMACS_KEY_BINDINGS_EXCEPTION = [
  ...emacs,
  ...vscode,
  ...remoteDesktop,
  ...terminal,
  ...vi,
  ...virtualMachine,
  ...x11,
];
