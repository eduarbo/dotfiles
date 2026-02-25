import { describe, it, expect } from 'vitest';
import { profile } from './profile.js';
import type { ComplexModifications } from './complexModifications.js';

describe('profile', () => {
  it('creates an empty profile with defaults', () => {
    const result = profile('Empty');
    expect(result.name).toBe('Empty');
    expect(result.devices).toEqual([]);
    expect(result.virtual_hid_keyboard).toEqual({ keyboard_type_v2: 'ansi' });
    expect(result.complex_modifications?.rules).toEqual([]);
    expect(result.complex_modifications?.parameters).toEqual({
      'basic.simultaneous_threshold_milliseconds': 50,
      'basic.to_delayed_action_delay_milliseconds': 500,
      'basic.to_if_alone_timeout_milliseconds': 500,
      'basic.to_if_held_down_threshold_milliseconds': 500,
    });
  });

  it('merges rules from multiple ComplexModifications', () => {
    const mod1: ComplexModifications = {
      title: 'Layer 1',
      rules: [
        {
          description: 'Rule 1',
          manipulators: [{ type: 'basic', from: { key_code: 'a' }, to: [{ key_code: 'b' }] }],
        },
      ],
    };
    const mod2: ComplexModifications = {
      title: 'Layer 2',
      rules: [
        {
          description: 'Rule 2',
          manipulators: [{ type: 'basic', from: { key_code: 'c' }, to: [{ key_code: 'd' }] }],
        },
      ],
    };
    const result = profile('Test', [mod1, mod2]);
    expect(result.complex_modifications?.rules).toHaveLength(2);
    expect(result.complex_modifications?.rules[0].description).toBe('Rule 1');
    expect(result.complex_modifications?.rules[1].description).toBe('Rule 2');
  });

  it('applies overrides while keeping defaults', () => {
    const result = profile('Selected', [], { selected: true });
    expect(result.selected).toBe(true);
    expect(result.name).toBe('Selected');
    expect(result.virtual_hid_keyboard).toEqual({ keyboard_type_v2: 'ansi' });
  });

  it('allows overriding devices', () => {
    const devices = [
      {
        identifiers: { vendor_id: 123 },
        disable_built_in_keyboard_if_exists: true,
        ignore: false,
        manipulate_caps_lock_led: true,
      },
    ];
    const result = profile('With Devices', [], { devices });
    expect(result.devices).toEqual(devices);
  });
});
