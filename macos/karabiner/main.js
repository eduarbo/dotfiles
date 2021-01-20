#!/usr/bin/env node

// ░█░█░█▀█░█▀▄░█▀█░█▀▄░▀█▀░█▀█░█▀▀░█▀▄░░░░░█▀▀░█░░░█▀▀░█▄█░█▀▀░█▀█░▀█▀░█▀▀
// ░█▀▄░█▀█░█▀▄░█▀█░█▀▄░░█░░█░█░█▀▀░█▀▄░▄▄▄░█▀▀░█░░░█▀▀░█░█░█▀▀░█░█░░█░░▀▀█
// ░▀░▀░▀░▀░▀░▀░▀░▀░▀▀░░▀▀▀░▀░▀░▀▀▀░▀░▀░░░░░▀▀▀░▀▀▀░▀▀▀░▀░▀░▀▀▀░▀░▀░░▀░░▀▀▀

// Generate main configuration and complex_modifications files


const fs = require('fs');
const path = require('path');
const os = require('os');

const { karabiner, complexModifications } = require('./config/karabiner');

const JSON_SPACE_INDENTATION = 2;
const HOME_PATH = os.homedir();
const DEFAULT_CONFIG_DIR = `${HOME_PATH}/.config/karabiner`;
const CONFIG_DIR = process.env.KARABINER_CONFIG_DIR || DEFAULT_CONFIG_DIR;

writeFile(`${CONFIG_DIR}/karabiner.json`, karabiner);

Object.entries(complexModifications).forEach(([name, mod]) => {
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
