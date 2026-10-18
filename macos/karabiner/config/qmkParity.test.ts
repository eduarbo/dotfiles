import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { describe, it, expect } from 'vitest';
import { complexModifications } from './karabiner.js';
import type { Manipulator } from '../lib/index.js';

// Compares the Karabiner layers with the QMK userspace that drives the Corne and
// Piantor. Set QMK_USERSPACE to point at another checkout; the suite is skipped
// when no checkout is available.
const qmkUserspace =
  process.env.QMK_USERSPACE ?? path.join(os.homedir(), 'dev/eduarbo/qmk_userspace');
const wrappersPath = path.join(qmkUserspace, 'users/eduarbo/wrappers.h');

const ROWS = ['L1', 'L2', 'L3', 'R1', 'R2', 'R3'] as const;
type Row = (typeof ROWS)[number];

// Built-in keyboard keys at each position of the 3x5 halves.
const POSITIONS: Record<Row, string[]> = {
  L1: ['q', 'w', 'e', 'r', 't'],
  L2: ['a', 's', 'd', 'f', 'g'],
  L3: ['z', 'x', 'c', 'v', 'b'],
  R1: ['y', 'u', 'i', 'o', 'p'],
  R2: ['h', 'j', 'k', 'l', 'semicolon'],
  R3: ['n', 'm', 'comma', 'period', 'slash'],
};

const LETTERS = Object.fromEntries(
  'abcdefghijklmnopqrstuvwxyz'.split('').map((letter) => [`KC_${letter.toUpperCase()}`, letter]),
);
const DIGITS = Object.fromEntries('1234567890'.split('').map((digit) => [`KC_${digit}`, digit]));
const FUNCTION_KEYS = Object.fromEntries(
  Array.from({ length: 12 }, (_, index) => [`KC_F${index + 1}`, `f${index + 1}`]),
);

// QMK keycode -> Karabiner key_code emitted for the same key.
const QMK_TO_KARABINER: Record<string, string> = {
  ...LETTERS,
  ...DIGITS,
  ...FUNCTION_KEYS,
  KC_SCLN: 'semicolon',
  KC_COMM: 'comma',
  KC_DOT: 'period',
  KC_BSPC: 'delete_or_backspace',
  KC_PGUP: 'page_up',
  KC_PGDN: 'page_down',
  KC_EQL: 'equal_sign',
  KC_MINS: 'hyphen',
  KC_BSLS: 'backslash',
  KC_LBRC: 'open_bracket',
  KC_RBRC: 'close_bracket',
  KC_SLSH: 'slash',
  KC_LEFT: 'left_arrow',
  KC_DOWN: 'down_arrow',
  KC_UP: 'up_arrow',
  KC_RIGHT: 'right_arrow',
  KC_GRV: 'grave_accent_and_tilde',
  KC_QUOT: 'quote',
  KC_PSCR: 'print_screen',
  KC_SCRL: 'locking_scroll_lock',
  KC_PAUS: 'pause',
  KC_VOLU: 'volume_increment',
  KC_VOLD: 'volume_decrement',
  KC_MUTE: 'mute',
  KC_MRWD: 'rewind',
  KC_MPLY: 'play_or_pause',
  KC_MFFD: 'fastforward',
  KC_BRID: 'display_brightness_decrement',
  KC_BRIU: 'display_brightness_increment',
  KC_MCTL: 'mission_control',
  // Accent mod-taps: the tap sends Option+E / Option+N.
  ACUTE_LSFT: 'e',
  TILDE_LSFT: 'n',
  // Documented difference: QMK Caps Word, Karabiner Caps Lock.
  CW_TOGG: 'caps_lock',
};

const parseWrappers = (source: string): Map<string, string[]> => {
  const blocks = new Map<string, string[]>();
  for (const line of source.split('\n')) {
    const match = /^#define\s+_+([A-Z0-9_]+?)_+\s+(\S.*)$/.exec(line.trim());
    if (match) {
      blocks.set(
        match[1],
        match[2].split(',').map((token) => token.trim()),
      );
    }
  }
  return blocks;
};

const keyCodeOf = (manipulator: Manipulator | undefined): string | undefined => {
  const events = manipulator?.to_if_alone ?? manipulator?.to ?? [];
  const event = events.find((candidate) => 'key_code' in candidate);
  return event && 'key_code' in event ? event.key_code : undefined;
};

const unshiftedBinding = (manipulators: Manipulator[], keyCode: string) =>
  manipulators.find(
    (manipulator) =>
      'key_code' in manipulator.from &&
      manipulator.from.key_code === keyCode &&
      !manipulator.from.modifiers?.mandatory?.includes('shift'),
  );

const toKarabiner = (token: string): string => {
  const keyCode = QMK_TO_KARABINER[token];
  if (!keyCode) {
    throw new Error(`No Karabiner equivalent for QMK keycode ${token}`);
  }
  return keyCode;
};

describe.skipIf(!fs.existsSync(wrappersPath))('QMK userspace parity', () => {
  const blocks = parseWrappers(
    fs.existsSync(wrappersPath) ? fs.readFileSync(wrappersPath, 'utf8') : '',
  );
  const qmkRow = (layer: string, row: Row) => {
    const tokens = blocks.get(`${layer}_${row}`);
    expect(tokens, `${layer}_${row} in wrappers.h`).toHaveLength(5);
    return tokens ?? [];
  };

  const baseManipulators =
    complexModifications.baseLayer.rules.find(
      (rule) => rule.description === 'BASE layer: Custom QWERTY',
    )?.manipulators ?? [];
  const baseOutput = (keyCode: string) =>
    keyCodeOf(unshiftedBinding(baseManipulators, keyCode)) ?? keyCode;

  it.each(ROWS)('matches the base layer on %s', (row) => {
    const qmk = qmkRow('QWERTY', row).map(toKarabiner);
    expect(POSITIONS[row].map(baseOutput)).toEqual(qmk);
  });

  it.each(ROWS)('matches the Symbols layer on %s', (row) => {
    const symbols = complexModifications.symbolsLayer.rules.flatMap((rule) => rule.manipulators);
    const base = qmkRow('QWERTY', row);
    // Transparent QMK keys fall through to the base layer.
    const qmk = qmkRow('SYMBOLS', row).map((token, index) =>
      toKarabiner(token === '_______' ? base[index] : token),
    );
    expect(POSITIONS[row].map((keyCode) => keyCodeOf(unshiftedBinding(symbols, keyCode)))).toEqual(
      qmk,
    );
  });

  it.each(ROWS)('matches the F-Keys layer on %s', (row) => {
    const fn = complexModifications.fnLayer.rules.flatMap((rule) => rule.manipulators);
    const qmk = qmkRow('F_KEYS', row).map(toKarabiner);
    expect(POSITIONS[row].map((keyCode) => keyCodeOf(unshiftedBinding(fn, keyCode)))).toEqual(qmk);
  });
});
