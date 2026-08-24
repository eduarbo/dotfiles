import { describe, it, expect } from 'vitest';
import { karabiner, complexModifications } from './karabiner.js';
import { GAMES } from '../lib/index.js';

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

  it('places the Civ rules before Base and keeps the general Gaming layer inactive', () => {
    const descriptions =
      karabiner.profiles[1].complex_modifications?.rules.map((rule) => rule.description) ?? [];
    const civIndex = descriptions.findIndex((description) => description.startsWith('Civ V layer:'));
    const baseIndex = descriptions.findIndex((description) => description.startsWith('BASE layer:'));

    expect(civIndex).toBeGreaterThanOrEqual(0);
    expect(baseIndex).toBeGreaterThanOrEqual(0);
    expect(civIndex).toBeLessThan(baseIndex);
    expect(descriptions.some((description) => description.startsWith('GAMING layer:'))).toBe(false);
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

  it('maps Caps Lock and quote to Shift while preserving their SYMBOLS outputs', () => {
    const customQwerty = complexModifications.baseLayer.rules.find(
      (rule) => rule.description === 'BASE layer: Custom QWERTY',
    );
    const symbolsManipulators = complexModifications.symbolsLayer.rules.flatMap(
      (rule) => rule.manipulators,
    );

    expect(customQwerty?.manipulators).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          from: expect.objectContaining({ key_code: 'caps_lock' }),
          to: [{ key_code: 'left_shift' }],
        }),
        expect.objectContaining({
          from: expect.objectContaining({ key_code: 'quote' }),
          to: [{ key_code: 'right_shift' }],
        }),
      ]),
    );
    expect(symbolsManipulators).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          from: expect.objectContaining({ key_code: 'z' }),
          to: [{ key_code: 'caps_lock' }],
        }),
        expect.objectContaining({
          from: expect.objectContaining({ key_code: 'm' }),
          to: [{ key_code: 'quote' }],
        }),
      ]),
    );
  });

  it('orders shifted Symbols bindings before their generic bindings', () => {
    const manipulators = complexModifications.symbolsLayer.rules.flatMap(
      (rule) => rule.manipulators,
    );
    const indexOf = (description: string) =>
      manipulators.findIndex((manipulator) => manipulator.description === description);

    expect(indexOf('from left_shift+right_shift+q to home')).toBeLessThan(
      indexOf('from right_shift+q to page_up'),
    );
    expect(indexOf('from left_shift+right_shift+p to end')).toBeLessThan(
      indexOf('from right_shift+p to page_down'),
    );
  });

  it('uses function keys on the left FN half and media controls on the right', () => {
    const manipulators = complexModifications.fnLayer.rules.flatMap((rule) => rule.manipulators);
    const bindingFor = (keyCode: string) =>
      manipulators.find(
        (manipulator) =>
          'key_code' in manipulator.from && manipulator.from.key_code === keyCode,
      );

    expect(bindingFor('q')?.to).toEqual(
      expect.arrayContaining([expect.objectContaining({ key_code: 'f12' })]),
    );
    expect(bindingFor('x')?.to).toEqual(
      expect.arrayContaining([expect.objectContaining({ key_code: 'f1' })]),
    );
    expect(bindingFor('y')?.to).toEqual(
      expect.arrayContaining([expect.objectContaining({ key_code: 'volume_increment' })]),
    );
    expect(bindingFor('m')?.to).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ key_code: 'display_brightness_decrement' }),
      ]),
    );
  });

  it('excludes games from Emacs keybindings', () => {
    const conditions = complexModifications.emacsKeybindings.rules
      .flatMap((rule) => rule.manipulators)
      .flatMap((manipulator) => manipulator.conditions ?? [])
      .filter((condition) => condition.type === 'frontmost_application_unless');

    expect(conditions.length).toBeGreaterThan(0);
    for (const condition of conditions) {
      expect(condition).toEqual(
        expect.objectContaining({ bundle_identifiers: expect.arrayContaining(GAMES) }),
      );
    }
  });

  it('keeps the Civ layer device-neutral', () => {
    const conditions = complexModifications.civLayer.rules
      .flatMap((rule) => rule.manipulators)
      .flatMap((manipulator) => manipulator.conditions ?? []);

    expect(conditions.some((condition) => condition.type.startsWith('device_'))).toBe(false);
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
