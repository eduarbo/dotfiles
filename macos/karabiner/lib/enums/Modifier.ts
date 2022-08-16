export const BASE_MODIFIERS = [
  'command',
  'option',
  'control',
  'shift',
] as const;

export const MODIFIERS = [
  'caps_lock',
  'left_command',
  'left_control',
  'left_option',
  'left_shift',
  'right_command',
  'right_control',
  'right_option',
  'right_shift',
  'fn',
  'command',
  'option',
  'control',
  'shift',
  'left_alt',
  'left_gui',
  'right_alt',
  'right_gui',
  'any',
] as const;

export type Modifier = typeof MODIFIERS[number];

export type BaseModifier = typeof BASE_MODIFIERS[number];
