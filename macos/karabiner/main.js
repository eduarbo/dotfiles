#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const karabiner = require('./karabiner');
const mods = require('./mods');

const JSON_SPACE_INDENTATION = 2;
const CONFIG_DIR = process.env.KARABINER_CONFIG_DIR || '~/.config/karabiner';

writeFile(`${CONFIG_DIR}/karabiner.json`, karabiner);

Object.entries(mods).forEach(([name, mod]) => {
  writeFile(`${CONFIG_DIR}/assets/complex_modifications/${name}.json`, mod);
});

function writeFile(file, data) {
  const d = JSON.stringify(data, null, JSON_SPACE_INDENTATION);

  return fs.promises.writeFile(file, d)
    .catch((error) => {
      // Error NO ENTry
      if (error.code !== 'ENOENT') throw error;

      // If directory doesn't exist, make it!
      fs.promises.mkdir(path.dirname(file), { recursive: true })
        .then(() => writeFile(file, data));
    });
}
