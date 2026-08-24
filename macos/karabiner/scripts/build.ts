import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
// This script runs from dist/scripts after TypeScript compilation.
const projectRoot = path.resolve(__dirname, '../..');

async function writeJSON(filePath: string, data: unknown): Promise<void> {
  const json = JSON.stringify(data, null, 2);
  await fs.promises.mkdir(path.dirname(filePath), { recursive: true });
  await fs.promises.writeFile(filePath, json + '\n');
}

async function main(): Promise<void> {
  const [outputDir] = process.argv.slice(2);

  if (!outputDir) {
    console.error('Usage: build <output-dir>');
    process.exit(1);
  }

  const outputPath = path.resolve(projectRoot, outputDir);

  const { karabiner, complexModifications } = await import(
    '../config/karabiner.js'
  );

  await writeJSON(path.join(outputPath, 'karabiner.json'), karabiner);

  const modEntries = Object.entries(
    complexModifications as Record<string, unknown>,
  );
  await Promise.all(
    modEntries.map(([name, mod]) =>
      writeJSON(
        path.join(outputPath, 'assets', 'complex_modifications', `${name}.json`),
        mod,
      ),
    ),
  );

  console.log(
    `Generated karabiner.json + ${modEntries.length} complex modification files in ${outputDir}/`,
  );
}

main();
