import { describe, it, expect } from 'vitest';
import { profile } from './profile.js';
import { withSelectedProfile } from './selectedProfile.js';
import type { KarabinerConfig } from './karabinerConfig.js';

const config: KarabinerConfig = {
  profiles: [profile('Empty'), profile('Default'), profile('Keebs', [], { selected: true })],
  global: {},
};

describe('withSelectedProfile', () => {
  it('moves the selection to the named profile', () => {
    const selected = withSelectedProfile(config, 'Default').profiles.map((p) => p.selected);
    expect(selected).toEqual([undefined, true, undefined]);
  });

  it('keeps the generated selection for unknown or missing names', () => {
    expect(withSelectedProfile(config, 'Gone')).toBe(config);
    expect(withSelectedProfile(config, undefined)).toBe(config);
  });
});
