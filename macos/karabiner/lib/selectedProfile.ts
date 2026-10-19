import type { KarabinerConfig } from './karabinerConfig.js';

/**
 * Marks `name` as the selected profile, so deploying a regenerated config keeps the
 * profile chosen in Karabiner's menu. Unknown or missing names leave the config as is.
 */
export const withSelectedProfile = (
  config: KarabinerConfig,
  name: string | undefined,
): KarabinerConfig => {
  if (!name || !config.profiles.some((profile) => profile.name === name)) return config;

  return {
    ...config,
    profiles: config.profiles.map(({ selected: _selected, ...profile }) =>
      profile.name === name ? { ...profile, selected: true } : profile,
    ),
  };
};
