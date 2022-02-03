// Generate main configuration and complex_modifications files

const fs = require('fs');
const path = require('path');

const { karabiner, complexModifications } = require('../dist/config/karabiner');

const JSON_SPACE_INDENTATION = 2;
const args = process.argv.slice(2);
const [outputDir] = args;

// Exit if no output directory is given for karabiner configuration files
if (!args.length) {
  // eslint-disable-next-line no-console
  console.error('Expected the output Dir as argument! bye 👋');
  process.exit(1);
}

async function writeFile(file, data) {
  const json = JSON.stringify(data, null, JSON_SPACE_INDENTATION);

  return fs.promises.writeFile(file, json).catch((error) => {
    // Error NO ENTry
    if (error.code !== 'ENOENT') throw error;

    // If directory doesn't exist, make it!
    fs.promises.mkdir(path.dirname(file), { recursive: true }).then(() => writeFile(file, data));
  });
}

writeFile(`${outputDir}/karabiner.json`, karabiner);

Object.entries(complexModifications).forEach(([name, mod]) => {
  writeFile(`${outputDir}/assets/complex_modifications/${name}.json`, mod);
});
