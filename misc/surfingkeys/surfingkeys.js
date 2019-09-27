// ************************* WARNING *************************
// We can't just copy statements from the default mappings file as the bound
// functions in that file may rely on some unstable functions/variables, which
// may be changed some day.
//
// Therefore, the best practice to remap is using map instead of mapkey, for
// example:
//
//      map('F', 'af');
//
// is better than
//
//      mapkey('F', '#1Open a link in new tab', () => Hints.create("", Hints.dispatchMouseClick, {tabbed: true}));
//
// ************************* WARNING *************************


//
// Settings

Object.assign(settings, {
  smoothScroll: false,
  createHintAlign: 'left',
  focusFirstCandidate: true,
});

Object.assign(Hints, {
  // Only left hand keys
  characters: 'asdfgqwertcvb',
});


//
// Helpers

const GROUP = {
  HELP: 0,
  MOUSE_CLICK: 1,
  SCROLL_PAGE: 2,
  TABS: 3,
  PAGE: 4,
  SESSIONS: 5,
  SEARCH: 6,
  CLIPBOARD: 7,
  OMNIBAR: 8,
  VISUAL: 9,
  VIM: 10,
  SETTINGS: 11,
  CHROME: 12,
  PROXY: 13,
  MISC: 14,
  INSERT: 15
};

function keymap(group, fn) {
  const bind = mapkeyFn => (keys, annotation, cb, options) => {
    [].concat(keys).forEach((key) => {
      mapkeyFn(key, `#${group}${annotation}`, cb, options);
    })
  }

  const helpers = {
    normal: bind(mapkey),
    visual: bind(vmapkey),
    insert: bind(imapkey),
  };

  return fn(helpers)
}


//
// Bindings

// I don't need them
unmap('sp');           // namespace for Proxy bindings
unmap('cp');           // toggle proxy for current site
unmap('sfr');          // show failed web requests of current page
unmap('se');           // edit settings
unmap('sm');           // preview markdown
unmap('<Ctrl-Alt-d>'); // settings
unmap('Z');            // namespace for Session bindings
unmap('sql');          // show last action
iunmap(':');           // emoji completion

// text navigation is already handled by Karabiner-Elements
iunmap('<Ctrl-e>');
iunmap('<Ctrl-f>');
iunmap('<Ctrl-u>');
iunmap('<Alt-b>');
iunmap('<Alt-f>');
iunmap('<Alt-w>');
iunmap('<Alt-d>');

keymap(GROUP.HELP, ({ insert }) => {
  map(',', '<Alt-i>');
  unmap('<Alt-i>');

  map('<Ctrl-q>', '<Alt-s>');
  insert('<Ctrl-q>', 'Toggle SurfingKeys on current site', () => Normal.toggleBlacklist());
  unmap('<Alt-s>');
});

keymap(GROUP.CHROME, ({ normal }) => {
  map('ca', 'ga');
  unmap('ga');

  map('cb', 'gb');
  unmap('gb');

  map('cc', 'gc');
  unmap('gc');

  map('cd', 'gd');
  unmap('gd');

  map('ck', 'gk');
  unmap('gk');

  map('ce', 'ge');
  unmap('ge');

  map('cn', 'gn');
  unmap('gn');

  map('ci', 'si');
  unmap('si');

  normal('ch', 'Open Chrome net-internals#hsts', () => tabOpenLink("chrome://net-internals/#hsts"));
  normal('cy', 'Open Chrome History', () => tabOpenLink("chrome://history/"));
});

keymap(GROUP.MOUSE_CLICK, () => {
  unmap('af');

  // Open multiple links in a new tab
  map('F', 'cf');
  unmap('cf');

  // Mouse out last element
  map('gm', ';m');
  unmap(';m');

  // Mouse over elements
  map('gh', '<Ctrl-h>');
  unmap('<Ctrl-h)');

  // Mouse out elements
  map('gH', '<Ctrl-j>');
  unmap('<Ctrl-j>');

  map('gq', 'cq');
  unmap('cq');
});

keymap(GROUP.SCROLL_PAGE, () => {
  // Scroll page up/down
  map('K', 'e');
  map('J', 'd');

  // Change scroll target
  map('gs', 'cs');
  unmap('cs');

  // Reset scroll target
  map('gS', 'cS');
  unmap('cS');
});

keymap(GROUP.TABS, () => {
  map('<Ctrl-h>', 'E'); // Go one tab left
  map('<Ctrl-l>', 'R'); // Go one tab right
  unmap('E');

  // Go back in history
  map('H', 'S');
  unmap('S');

  // Go forward in history
  map('L', 'D');
  unmap('D');

  // Go to last used tab
  map('`', '<Ctrl-6>');
  unmap('<Ctrl-6>');

  // pin/unpin current tab
  map('gp', '<Alt-p>')
  unmap('<Alt-p>');

  // mute/unmute current tab
  map('gm', '<Alt-m>')
  unmap('<Alt-m>');

  map('<', '<<'); // Move current tab to left
  map('>', '>>'); // Move current tab to right

  map('t', 'T'); // Choose a tab
  map('T', 'X'); // Restore closed tab
});

keymap(GROUP.PAGE, ({ normal }) => {
  normal('R', 'Reload the page without cache', () => RUNTIME('reloadTab', { nocache: true }));

  map('gl', 'sU');
  map('gL', 'su');
  unmap('su');
  unmap('sU');
});

const firingWallClassName = 'sk_firing_wall';
keymap(GROUP.MISC, ({ normal }) => {
  unmap('b');
  normal('bo', 'Open a bookmark', () => {
    Front.openOmnibar({ type: 'Bookmarks', tabbed: false });
  });
  normal('bO', 'Open a bookmark in new tab', () => {
    Front.openOmnibar({ type: 'Bookmarks' });
  });
  normal('bd', 'Remove bookmark for current page', () => RUNTIME('removeBookmark'));
  normal('ba', 'Bookmark current page to selected folder', () => {
    const extra = { url: window.location.href, title: document.title };
    Front.openOmnibar(({ type: "AddBookmark", extra }));
  });

  normal('gk', 'Kill element', () => killElement());
  normal('gK', 'Kill multiple elements', () => killElement({ multipleHits: true }));
  injectKillElementHintStyle();

  function createHint(hintOptions) {
    const { multipleHits } = hintOptions || {};

    Hints.create('*', (element) => {
      element.parentNode.removeChild(element);

      if (multipleHits) {
        setTimeout(() => createHint(hintOptions))
      } else {
        handleHintsExit();
      };
    });
  }

  function killElement(hintOptions) {
    document.body.classList.add(firingWallClassName);
    document.addEventListener('keydown', handleEsc);

    createHint(hintOptions);
  }

  function handleHintsExit() {
    document.body.classList.remove(firingWallClassName)
    document.removeEventListener('keydown', handleEsc);
  }

  // FIXME there is an existing event listener for keydown that takes place
  // before this so we need to press ESC twice to restore the styling and exit
  function handleEsc(event) {
    if (event.key === 'Escape') handleHintsExit();
  };

  function injectKillElementHintStyle() {
    const $css = document.createElement('style');
    $css.type = 'text/css';

    const styles = `.${firingWallClassName} * { outline: 1px dashed red; }`;
    $css.appendChild(document.createTextNode(styles));
    const $head = document.querySelector('head');
    $head.appendChild($css);
  }
});

keymap(GROUP.INSERT, () => {});

keymap(GROUP.VISUAL, () => {
  map('gv', 'V'); // Restore visual mode
  map('V', 'zv'); // Enter visual mode, and select whole element
  unmap('zv');
});

keymap(GROUP.CLIPBOARD, () => {
  // NOTE Using _ as a temporary variable to swap key bindings
  map('_', 'yf');
  map('yf', 'ya'); // Copy a link URL to the clipboard
  map('ya', '_'); // Copy form data in JSON on current page
  unmap('_');

  // Copy multiple link URLs to the clipboard
  map('yF', 'yma');
  unmap('yma');

  // Yank text of multiple elements
  map('yV', 'ymv');
  unmap('ymv');

  // Copy multiple columns of a table
  map('yC', 'ymc');
  unmap('ymc');
});

keymap(GROUP.VIM, () => {
  // Duplicate keymap
  unmap('<Ctrl-\'>');
})

keymap(GROUP.OMNIBAR, ({ normal }) => {
  unmap('o');

  normal('ot', 'Choose a tab with omnibar', () => {
    Front.openOmnibar({ type: 'Tabs' });
  });

  openOmnibarCombo('a', 'Open a URL', { type: 'URLs', extra: 'getAllSites' });
  openOmnibarCombo('x', 'Open recently closed URL', { type: 'URLs', extra: 'getRecentlyClosed' });
  openOmnibarCombo('u', 'Open URL from tab history', { type: 'URLs', extra: 'getTabURLs' });
  openOmnibar(';', 'Open commands', { type: "Commands" });

  const prefix = 'o';
  openOmnibarCombo('a', 'Open a URL', { prefix, type: 'URLs', extra: 'getAllSites' });
  openOmnibarCombo('x', 'Open recently closed URL', { prefix, type: 'URLs', extra: 'getRecentlyClosed' });
  openOmnibarCombo('u', 'Open URL from tab history', { prefix, type: 'URLs', extra: 'getTabURLs' });
  openOmnibarCombo('b', 'Open a bookmark', { prefix, type: 'Bookmarks' });
  openOmnibarCombo('m', 'Open URL from vim-like marks', { prefix, type: 'VIMarks' });
  openOmnibarCombo('y', 'Open URL from history', { prefix, type: 'History' });
  normal(`${prefix}i`, 'Open incognito window', () => {
    runtime.command({ action: 'openIncognito', url: window.location.href });
  });


  // Helpers

  function openOmnibar(key, annotation, options) {
    normal(key, annotation, () => {
      Front.openOmnibar(options);
    });
  }

  function openOmnibarCombo(key, annotation, options) {
    const { prefix = '', ...opts } = options;

    openOmnibar(`${prefix}${key}`, annotation, { ...opts, tabbed: false });
    openOmnibar(`${prefix}${key.toUpperCase()}`, `${annotation} in new tab`, opts);
  }
});


//
// Theme

const monospaceFontFamily = 'Hack, Lucida Console, Courier, monospace';
const fontFamily = 'system-ui, Helvetica, Verdana, Arial, sans-serif';

// Colors
const white = '#F0F4F2';
const lightGray = '#a7aba9';
const gray = '#696b6a';
const darkGray = '#454746';
const black = '#282c2f';
const lightBlack = '#3c4043';
const aquamarine = '#24ddb2';
const yellow = '#fece48';
const lightYellow = '#fcdc7c';
const green = '#A6F772';
const lightGreen = '#C6F9A5';
const darkGreen = '#6A9E49';

Hints.style(`
  font-family: ${monospaceFontFamily};
`);

Hints.style(`
  font-family: ${fontFamily};
  border-color: ${darkGreen};
  background: linear-gradient(0deg, ${green}, ${lightGreen});
`, 'text');

settings.theme = `
.sk_theme {
  font-family: ${fontFamily};
  font-size: 10pt;
  background: ${black};
  color: ${lightGray};
  line-height: 1.2rem;
}
.sk_theme input {
    color: ${white};
}
.sk_theme .feature_name {
  color: ${aquamarine};
  margin: 2rem 0 0.5rem;
}
.sk_theme .annotation {
  color: ${lightGray};
}
.sk_theme .omnibar_highlight {
  color: ${aquamarine};
}
.sk_theme .omnibar_folder {
  color: ${lightYellow};
}
.sk_theme .omnibar_visitcount,
.sk_theme .omnibar_timestamp,
.sk_theme .omnibar_folder,
#sk_omnibarSearchResult li div.url {
  font-family: ${monospaceFontFamily};
}
.sk_theme .omnibar_visitcount,
.sk_theme .omnibar_timestamp {
  color: ${darkGray};
  font-size: 0.75rem;
}
.sk_theme .focused .omnibar_visitcount,
.sk_theme .focused .omnibar_timestamp {
  color: ${gray};
}
#sk_omnibarSearchResult li div.url {
  color: ${gray};
  font-weight: normal;
}
.sk_theme #sk_omnibarSearchResult>ul>li:nth-child(odd) {
  background: ${black};
}
.sk_theme #sk_omnibarSearchResult>ul>li.focused {
  color: ${white};
  background: ${lightBlack};
}
.sk_theme #sk_omnibarSearchResult>ul>li.focused .url {
  color: ${lightGray};
}
.sk_theme .separator {
  display: none;
}
#sk_usage {
  background: ${black};
  color: ${lightGray};
  padding: 0 1rem;
}
#sk_usage .feature_name>span {
  color: ${aquamarine};
  padding-bottom: 2px;
  border-bottom: 2px solid ${yellow};
  font-size: 1rem;
}
#sk_omnibarSearchArea>input {
  padding: 0;
  margin-left: 0.5rem;
}
#sk_omnibarSearchResult {
  margin: 0;
}
.sk_omnibar_middle #sk_omnibarSearchResult>ul {
  margin: 0;
}
#sk_omnibarSearchResult>ul>li {
  padding: 0.25rem 0.5rem;
}
#sk_omnibarSearchResult li div.title {
  font-size: 0.875rem;
}
#sk_omnibarSearchResult li div.url {
  line-height: 1rem;
}
#sk_status {
  padding: 0.25rem .8rem 0.25rem;
}
#sk_status,
#sk_find {
  font-size: 1rem;
  border-bottom: none;
}
#sk_status>span {
  padding: 0 !important;
}
#sk_status>span:first-child {
  border: none !important;
  color: ${aquamarine};
  font-weight: bold;
}
.sk_omnibar_middle #sk_omnibarSearchArea {
  margin: 0.5rem;
  border-bottom: none;
  align-items: baseline;
}
`;
