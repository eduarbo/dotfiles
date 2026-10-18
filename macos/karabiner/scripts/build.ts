import fs from 'node:fs';
import os from 'node:os';
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

// Karabiner watches its config directory, not the symlink target, so touch the
// deployed symlink when it points at the regenerated file to make Karabiner reload.
async function notifyKarabiner(generatedPath: string): Promise<void> {
  const configDir =
    process.env.KARABINER_CONFIG_DIR ??
    path.join(process.env.XDG_CONFIG_HOME ?? path.join(os.homedir(), '.config'), 'karabiner');
  const deployedPath = path.join(configDir, 'karabiner.json');

  try {
    if (!(await fs.promises.lstat(deployedPath)).isSymbolicLink()) return;
    const [deployedTarget, generatedTarget] = await Promise.all([
      fs.promises.realpath(deployedPath),
      fs.promises.realpath(generatedPath),
    ]);
    if (deployedTarget !== generatedTarget) return;
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === 'ENOENT') return;
    throw error;
  }

  const now = new Date();
  await fs.promises.lutimes(deployedPath, now, now);
  console.log(`Touched ${deployedPath} so Karabiner reloads it`);
}

async function main(): Promise<void> {
  const [outputDir] = process.argv.slice(2);

  if (!outputDir) {
    console.error('Usage: build <output-dir>');
    process.exit(1);
  }

  const outputPath = path.resolve(projectRoot, outputDir);

  const { karabiner, complexModifications } = await import('../config/karabiner.js');

  const karabinerPath = path.join(outputPath, 'karabiner.json');
  await writeJSON(karabinerPath, karabiner);

  const modEntries = Object.entries(complexModifications as Record<string, unknown>);
  await Promise.all(
    modEntries.map(([name, mod]) =>
      writeJSON(path.join(outputPath, 'assets', 'complex_modifications', `${name}.json`), mod),
    ),
  );

  console.log(
    `Generated karabiner.json + ${modEntries.length} complex modification files in ${outputDir}/`,
  );

  await notifyKarabiner(karabinerPath);
}

main();
