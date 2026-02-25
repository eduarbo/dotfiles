import { describe, it, expect } from 'vitest';
import { ignoreCrkbd, ignoreBeekeb, ignoreGames, ignoreKeebs } from './conditions.js';
import { CRKBD, BEEKEEB, GAMES } from './consts/index.js';

describe('ignoreCrkbd', () => {
  it('creates a device_unless condition for CRKBD vendor', () => {
    expect(ignoreCrkbd).toEqual({
      type: 'device_unless',
      identifiers: [{ vendor_id: CRKBD.vendor_id }],
    });
  });
});

describe('ignoreBeekeb', () => {
  it('creates a device_unless condition for BEEKEEB', () => {
    expect(ignoreBeekeb).toEqual({
      type: 'device_unless',
      identifiers: [BEEKEEB],
    });
  });
});

describe('ignoreGames', () => {
  it('creates a frontmost_application_unless condition for games', () => {
    expect(ignoreGames).toEqual({
      type: 'frontmost_application_unless',
      bundle_identifiers: GAMES,
    });
  });
});

describe('ignoreKeebs', () => {
  it('combines CRKBD and BEEKEEB conditions', () => {
    expect(ignoreKeebs).toHaveLength(2);
    expect(ignoreKeebs[0]).toBe(ignoreCrkbd);
    expect(ignoreKeebs[1]).toBe(ignoreBeekeb);
  });
});
