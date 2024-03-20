import type { KeyCode, FunctionKeyCode } from './enums';
import type { ComplexModifications, Rule } from './complexModifications';
import type { ManipulatorParameters } from './manipulator';
import type { Identifier } from './conditions';

interface SimpleModification {
  from: {
    key_code: KeyCode;
  };
  to: {
    key_code: KeyCode;
  };
}

interface FnFunctionKey {
  from: {
    key_code: FunctionKeyCode;
  };
  to: {
    key_code: KeyCode;
  };
}

interface Device {
  disable_built_in_keyboard_if_exists: boolean;
  fn_function_keys: FnFunctionKey[];
  identifiers: Identifier[];
  ignore: boolean;
  manipulate_caps_lock_led: boolean;
  simple_modifications: SimpleModification[];
}

export interface Profile {
  name: string;
  selected?: boolean;
  simple_modifications?: SimpleModification[];
  fn_function_keys?: FnFunctionKey[];
  complex_modifications?: {
    parameters: ManipulatorParameters;
    rules: Rule[];
  };
  virtual_hid_keyboard?: {
    country_code: number;
    indicate_sticky_modifier_keys_state: boolean;
    mouse_key_xy_scale: number;
  };
  devices?: Device[];
  parameters?: {
    delay_milliseconds_before_open_device: number;
  };
}

const getRules = (complexModifications: ComplexModifications[]) =>
  complexModifications.reduce(
    (acc: Rule[], complexModification: ComplexModifications) => [
      ...acc,
      ...complexModification.rules,
    ],
    [],
  );

export const profile = (
  name: string,
  complexModifications: ComplexModifications[] = [],
  overrides = {},
): Profile => ({
  name,
  devices: [],
  ...overrides,
  complex_modifications: {
    parameters: {
      'basic.simultaneous_threshold_milliseconds': 50,
      'basic.to_delayed_action_delay_milliseconds': 500,
      'basic.to_if_alone_timeout_milliseconds': 500,
      'basic.to_if_held_down_threshold_milliseconds': 500,
    },
    rules: getRules(complexModifications),
  },
});
