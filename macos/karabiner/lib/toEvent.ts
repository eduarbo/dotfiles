import type {
  Modifier,
  KeyCode,
  PointingButton,
  ConsumerKeyCode,
  StickyModifierState,
} from './enums';

export interface ToEventCommon {
  modifiers?: Modifier[];
  lazy?: boolean;
  repeat?: boolean;
  halt?: boolean;
  hold_down_milliseconds?: number;
}

export type ToKeyCodeEvent = ToEventCommon & {
  key_code: KeyCode;
};

type ToConsumerKeyCodeEvent = ToEventCommon & {
  consumer_key_code: ConsumerKeyCode;
};

type ToPointingButtonEvent = ToEventCommon & {
  pointing_button: PointingButton;
};

type ToShellCommandEvent = ToEventCommon & {
  shell_command: string;
};

interface SelectInputSource {
  language?: string;
  input_source_id?: string;
  input_mode_id?: string;
}

type ToSelectInputSourceEvent = ToEventCommon & {
  select_input_source: SelectInputSource;
};

export type VariableName = string;

export type VariableValue = string | number | boolean;

export interface Variable {
  name: VariableName;
  value: VariableValue;
}

type ToSetVariableEvent = ToEventCommon & {
  set_variable: Variable;
};

interface MouseKey {
  x?: number;
  y?: number;
  vertical_wheel?: number;
  horizontal_wheel?: number;
  speed_multiplier?: number;
}

type ToMouseKeyEvent = ToEventCommon & {
  mouse_key: MouseKey;
};

type StickyModifier = Partial<Record<Modifier, StickyModifierState>>;

type ToStickyModifierEvent = ToEventCommon & {
  sticky_modifier: StickyModifier;
};

export type ToEvent =
  | ToKeyCodeEvent
  | ToConsumerKeyCodeEvent
  | ToPointingButtonEvent
  | ToShellCommandEvent
  | ToSelectInputSourceEvent
  | ToSetVariableEvent
  | ToMouseKeyEvent
  | ToStickyModifierEvent;

export type ToKeyCodeTuple = [KeyCode, Modifier[]?];

export type VariableEvents = {
  to?: VariableValue;
  to_after_key_up?: VariableValue;
  to_if_alone?: VariableValue;
  to_if_held_down?: VariableValue;
};
export type SetVariables = { [name: string]: VariableEvents };
export type ToSetVariableTuple = [VariableName, VariableValue];

export const toKeyCode = (
  [key_code, modifiers]: ToKeyCodeTuple,
  options: ToEventCommon = {},
): ToKeyCodeEvent => ({
  key_code,
  modifiers,
  ...options,
});

export const toConsumerKeyCode = (
  [consumer_key_code, modifiers]: [ConsumerKeyCode, Modifier[]?],
  options: ToEventCommon = {},
): ToConsumerKeyCodeEvent => ({
  consumer_key_code,
  modifiers,
  ...options,
});

export const toPointingButton = (
  [pointing_button, modifiers]: [PointingButton, Modifier[]?],
  options: ToEventCommon = {},
): ToPointingButtonEvent => ({
  pointing_button,
  modifiers,
  ...options,
});

export const toShellCommand = (
  shell_command: string,
  options: ToEventCommon = {},
): ToShellCommandEvent => ({
  shell_command,
  ...options,
});

export const toSelectInputSource = (
  select_input_source: SelectInputSource,
  options: ToEventCommon = {},
): ToSelectInputSourceEvent => ({
  select_input_source,
  ...options,
});

export const toSetVariable = (
  [name, value]: ToSetVariableTuple,
  options: ToEventCommon = {},
): ToSetVariableEvent => ({
  set_variable: {
    name,
    value,
  },
  ...options,
});

export const toMouseKey = (mouse_key: MouseKey, options: ToEventCommon = {}): ToMouseKeyEvent => ({
  mouse_key,
  ...options,
});

export const toStickyModifier = (
  sticky_modifier: StickyModifier,
  options: ToEventCommon = {},
): ToStickyModifierEvent => ({
  sticky_modifier,
  ...options,
});

export const toHyperKeyCodeTuple: ToKeyCodeTuple = [
  'right_shift',
  ['option', 'command', 'control'],
];

export const toMehKeyCodeTuple: ToKeyCodeTuple = ['right_shift', ['option', 'control']];

export const toSuperKeyCodeTuple: ToKeyCodeTuple = ['right_command', ['option', 'control']];

// TODO software_function
