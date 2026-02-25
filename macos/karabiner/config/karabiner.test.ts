import { describe, it, expect } from 'vitest';
import { karabiner, complexModifications } from './karabiner.js';

describe('karabiner config', () => {
  it('has global settings', () => {
    expect(karabiner.global).toEqual({
      check_for_updates_on_startup: true,
      show_in_menu_bar: true,
      show_profile_name_in_menu_bar: false,
      indicate_sticky_modifier_keys_state: true,
    });
  });

  it('has three profiles', () => {
    expect(karabiner.profiles).toHaveLength(3);
  });

  it('first profile is Empty with no rules', () => {
    const empty = karabiner.profiles[0];
    expect(empty.name).toBe('Empty');
    expect(empty.complex_modifications?.rules).toEqual([]);
  });

  it('second profile is Default with rules', () => {
    const defaultProfile = karabiner.profiles[1];
    expect(defaultProfile.name).toBe('Default');
    expect(defaultProfile.complex_modifications?.rules.length).toBeGreaterThan(0);
  });

  it('third profile has CRKBD device config and is selected', () => {
    const crkbdProfile = karabiner.profiles[2];
    expect(crkbdProfile.selected).toBe(true);
    expect(crkbdProfile.devices?.length).toBeGreaterThan(0);
    expect(crkbdProfile.devices?.[0].disable_built_in_keyboard_if_exists).toBe(true);
  });

  it('all profiles have ANSI keyboard type', () => {
    for (const prof of karabiner.profiles) {
      expect(prof.virtual_hid_keyboard?.keyboard_type_v2).toBe('ansi');
    }
  });

  it('all profiles have timing parameters', () => {
    for (const prof of karabiner.profiles) {
      const params = prof.complex_modifications?.parameters;
      expect(params?.['basic.to_if_alone_timeout_milliseconds']).toBe(500);
      expect(params?.['basic.simultaneous_threshold_milliseconds']).toBe(50);
    }
  });
});

describe('complexModifications exports', () => {
  it('exports all expected layers', () => {
    expect(complexModifications.baseLayer).toBeDefined();
    expect(complexModifications.symbolsLayer).toBeDefined();
    expect(complexModifications.superLayer).toBeDefined();
    expect(complexModifications.fnLayer).toBeDefined();
    expect(complexModifications.emacsKeybindings).toBeDefined();
    expect(complexModifications.civLayer).toBeDefined();
    expect(complexModifications.gamingLayer).toBeDefined();
  });

  it('each layer has a title and rules', () => {
    const layers = [
      complexModifications.baseLayer,
      complexModifications.symbolsLayer,
      complexModifications.superLayer,
      complexModifications.fnLayer,
      complexModifications.emacsKeybindings,
      complexModifications.civLayer,
      complexModifications.gamingLayer,
    ];
    for (const layer of layers) {
      expect(layer.title).toBeTruthy();
      expect(layer.rules.length).toBeGreaterThan(0);
      for (const rule of layer.rules) {
        expect(rule.description).toBeTruthy();
        expect(rule.manipulators.length).toBeGreaterThan(0);
      }
    }
  });
});

describe('generated JSON structure', () => {
  it('produces valid JSON matching Karabiner format', () => {
    const json = JSON.parse(JSON.stringify(karabiner));
    expect(json).toHaveProperty('profiles');
    expect(json).toHaveProperty('global');

    for (const prof of json.profiles) {
      expect(prof).toHaveProperty('name');
      expect(prof).toHaveProperty('complex_modifications');
      expect(prof.complex_modifications).toHaveProperty('parameters');
      expect(prof.complex_modifications).toHaveProperty('rules');

      for (const rule of prof.complex_modifications.rules) {
        expect(rule).toHaveProperty('description');
        expect(rule).toHaveProperty('manipulators');
        for (const m of rule.manipulators) {
          expect(m.type).toBe('basic');
          expect(m).toHaveProperty('from');
        }
      }
    }
  });

  it('snapshot: full config structure', () => {
    expect(karabiner).toMatchSnapshot();
  });
});
