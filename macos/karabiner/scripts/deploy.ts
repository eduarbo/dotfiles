import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { withSelectedProfile } from '../lib/index.js';
import type { KarabinerConfig } from '../lib/index.js';

const projectRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');

/**
 * Copies the generated karabiner.json into Karabiner's config directory, keeping the
 * profile selected there. Karabiner rewrites its config whenever a profile is picked in
 * its menu, so a copy survives where a symlink would be replaced.
 */
export async function deployKarabiner(generatedPath: string): Promise<void> {
  const configDir =
    process.env.KARABINER_CONFIG_DIR ??
    path.join(process.env.XDG_CONFIG_HOME ?? path.join(os.homedir(), '.config'), 'karabiner');
  if (!fs.existsSync(configDir)) {
    console.log(`Skipped deploy: ${configDir} does not exist`);
    return;
  }

  const deployedPath = path.join(configDir, 'karabiner.json');
  const generated = JSON.parse(
    await fs.promises.readFile(generatedPath, 'utf8'),
  ) as KarabinerConfig;

  let selected: string | undefined;
  try {
    const deployed = JSON.parse(
      await fs.promises.readFile(deployedPath, 'utf8'),
    ) as KarabinerConfig;
    selected = deployed.profiles?.find((profile) => profile.selected)?.name;
  } catch (error) {
    if (!(error instanceof SyntaxError) && (error as NodeJS.ErrnoException).code !== 'ENOENT') {
      throw error;
    }
  }

  // Write next to the target and rename, so Karabiner never reads a partial file and a
  // legacy symlink is replaced rather than followed.
  const temporaryPath = `${deployedPath}.tmp`;
  const config = withSelectedProfile(generated, selected);
  await fs.promises.writeFile(temporaryPath, JSON.stringify(config, null, 2) + '\n', {
    mode: 0o600,
  });
  await fs.promises.rename(temporaryPath, deployedPath);
  console.log(
    `Deployed ${deployedPath}${selected ? ` with the "${selected}" profile selected` : ''}`,
  );
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  await deployKarabiner(path.join(projectRoot, 'json', 'karabiner.json'));
}
