import { BEEKEEB, CRKBD, GAMES } from './consts';
import type { VariableType } from './enums';
import type { Variable } from './toEvent';

export interface Identifier {
  vendor_id?: number;
  product_id?: number;
  location_id?: number;
  is_keyboard?: boolean;
  is_pointing_device?: boolean;
  is_touch_bar?: boolean;
}

export interface FrontmostApplicationCondition {
  type: 'frontmost_application_if' | 'frontmost_application_unless';
  bundle_identifiers?: string[];
  file_paths?: string[];
  description?: string;
}

export interface DeviceCondition {
  type: 'device_if' | 'device_unless';
  identifiers: Identifier[];
  description?: string;
}

export interface VariableCondition extends Variable {
  type: VariableType;
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

interface EventChangedCondition {
  type: 'event_changed_if' | 'event_changed_unless';
  value: boolean;
  description?: string;
}

export type Conditions =
  | FrontmostApplicationCondition
  | DeviceCondition
  | KeyboardTypeCondition
  | InputSourceCondition
  | VariableCondition
  | EventChangedCondition;

export const ignoreCrkbd: Conditions = {
  type: 'device_unless',
  identifiers: [
    {
      vendor_id: CRKBD.vendor_id,
    },
  ],
};

export const ignoreBeekeb: Conditions = {
  type: 'device_unless',
  identifiers: [BEEKEEB],
};

export const ignoreGames: Conditions = {
  type: 'frontmost_application_unless',
  bundle_identifiers: GAMES,
};

export const ignoreKeebs: Conditions[] = [ignoreCrkbd, ignoreBeekeb];
