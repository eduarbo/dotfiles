import { fromKeyCode } from './fromEvent';
import { toKeyCode } from './toEvent';

import type { FromEvent, FromKeyCodeTuple, FromEventCommon } from './fromEvent';
import type { ToEvent, ToEventCommon, ToKeyCodeTuple, Variable } from './toEvent';

interface FrontmostApplicationCondition {
  type: 'frontmost_application_if' | 'frontmost_application_unless';
  bundle_identifiers?: string[];
  file_paths?: string[];
  description?: string;
}

export interface Identifier {
  vendor_id?: number;
  product_id?: number;
  location_id?: number;
  is_keyboard?: boolean;
  is_pointing_device?: boolean;
  is_touch_bar?: boolean;
}

interface DeviceCondition {
  type: 'device_if' | 'device_unless';
  identifiers: Identifier[];
  description?: string;
}

interface KeyboardTypeCondition {
  type: 'keyboard_type_if' | 'keyboard_type_unless';
  // keyboard_types are joined by "or"
  keyboard_types: ['ansi' | 'iso' | 'jis'];
  description?: string;
}

interface InputSourceCondition {
  type: 'input_source_if' | 'input_source_unless';
  input_sources: {
    language?: string;
    input_source_id?: string;
    input_mode_id?: string;
  }[];
  description?: string;
}

interface VariableCondition extends Variable {
  type: 'variable_if' | 'variable_unless';
  description?: string;
}

interface EventChangedCondition {
  type: 'event_changed_if' | 'event_changed_unless';
  value: boolean;
  description?: string;
}

type Conditions =
  | FrontmostApplicationCondition
  | DeviceCondition
  | KeyboardTypeCondition
  | InputSourceCondition
  | VariableCondition
  | EventChangedCondition;

export interface ManipulatorParameters {
  'basic.simultaneous_threshold_milliseconds': number;
  'basic.to_delayed_action_delay_milliseconds': number;
  'basic.to_if_alone_timeout_milliseconds': number;
  'basic.to_if_held_down_threshold_milliseconds': number;
}

export interface ManipulatorOptions {
  to?: ToEvent[];
  to_if_alone?: ToEvent[];
  to_if_held_down?: ToEvent[];
  to_after_key_up?: ToEvent[];
  to_delayed_action?: {
    to_if_invoked?: ToEvent[];
    to_if_canceled?: ToEvent[];
  };
  conditions?: Conditions[];
  parameters?: ManipulatorParameters;
  description?: string;
}

export interface Manipulator extends ManipulatorOptions {
  // TODO add support for mouse_motion_to_scroll
  // https://karabiner-elements.pqrs.org/docs/json/complex-modifications-manipulator-definition/other-types/mouse-motion-to-scroll/
  type: 'basic';
  from: FromEvent;
}

//   description: 'open_bracket to left_option+e',

const keyToString = (...args: FromKeyCodeTuple[]) =>
  args
    .map(([keyCode, modifiers]) =>
      [modifiers && `${modifiers.join('+')}+`, keyCode].filter(Boolean).join(''),
    )
    .join(' | ');

export const manipulator = (
  fromEvent: FromEvent | FromKeyCodeTuple,
  options: ManipulatorOptions,
): Manipulator => ({
  type: 'basic',
  from: Array.isArray(fromEvent) ? fromKeyCode(fromEvent) : fromEvent,
  ...options,
});

export const remap = (
  fromTuple: FromKeyCodeTuple,
  toTuples: ToKeyCodeTuple[],
  options?: ManipulatorOptions,
): Manipulator => {
  const fromKeyText = keyToString(fromTuple);
  const toKeyText = keyToString(...toTuples);

  return {
    type: 'basic',
    from: fromKeyCode(fromTuple),
    to: toTuples.map((toTuple) => toKeyCode(toTuple)),
    description: fromKeyText && toKeyText && `from ${fromKeyText} to ${toKeyText}`,
    ...options,
  };
};

export const command = (fromTuple: FromKeyCodeTuple, shell_command: string): Manipulator => {
  const fromKeyText = keyToString(fromTuple);

  return {
    type: 'basic',
    from: fromKeyCode(fromTuple),
    to: [
      {
        shell_command,
      },
    ],
    description: `from ${fromKeyText} to shell command "${shell_command}"`,
  };
};

// `modTap` acts like a modifier when held, and a regular keycode when tapped. In
// other words, you can have a key that sends Escape when you tap it, but
// functions as a Control or Shift key when you hold it down.
export const modTap = (
  fromTuple: FromKeyCodeTuple,
  toTuples: ToKeyCodeTuple[],
  toTuplesOnTap: ToKeyCodeTuple[],
  options: {
    manipulatorOptions?: ManipulatorOptions;
    toOptions?: ToEventCommon;
    fromOptions?: FromEventCommon;
  } = {},
): Manipulator => {
  const fromKeyText = keyToString(fromTuple);
  const toKeyText = keyToString(...toTuples);
  const toKeyOnTapText = keyToString(...toTuplesOnTap);
  const { manipulatorOptions, toOptions, fromOptions } = options;

  return {
    type: 'basic',
    from: fromKeyCode(fromTuple, fromOptions),
    to: toTuples.map((toTuple) => toKeyCode(toTuple, toOptions)),
    to_if_alone: toTuplesOnTap.map((toTupleOnTap) => toKeyCode(toTupleOnTap)),
    description: `${fromKeyText} to ${toKeyText}, send ${toKeyOnTapText} on tap`,
    ...manipulatorOptions,
  };
};

export const lazyModTap = (
  fromTuple: FromKeyCodeTuple,
  toTuples: ToKeyCodeTuple[],
  toTuplesOnTap: ToKeyCodeTuple[],
  options: {
    manipulatorOptions?: ManipulatorOptions;
    toOptions?: ToEventCommon;
    fromOptions?: FromEventCommon;
  } = {},
) => {
  const { manipulatorOptions, toOptions, fromOptions } = options;
  return modTap(fromTuple, toTuples, toTuplesOnTap, {
    manipulatorOptions,
    toOptions: {
      lazy: true,
      ...toOptions,
    },
    fromOptions,
  });
};
