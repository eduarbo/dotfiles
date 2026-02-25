import { fromKeyCode } from './fromEvent.js';
import { toKeyCode, toSetVariable } from './toEvent.js';
import type { Condition } from './conditions.js';
import type { FromEvent, FromKeyCodeTuple, FromEventCommon } from './fromEvent.js';
import type { ToEvent, ToEventCommon, ToKeyCodeTuple, SetVariables } from './toEvent.js';

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
  conditions?: Condition[];
  parameters?: Partial<ManipulatorParameters>;
  description?: string;
}

export interface Manipulator extends ManipulatorOptions {
  type: 'basic';
  from: FromEvent;
}

export interface RemapOptions {
  manipulatorOptions?: ManipulatorOptions;
  toOptions?: ToEventCommon;
  setVariables?: SetVariables;
  fromOptions?: FromEventCommon;
}

const keyToString = (...args: FromKeyCodeTuple[]): string =>
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

interface VariableCollector {
  toVariables: ToEvent[];
  toAfterKeyUpVariables: ToEvent[];
  toIfAloneVariables: ToEvent[];
  toIfHeldDownVariables: ToEvent[];
}

const collectVariables = (setVariables: SetVariables = {}): VariableCollector =>
  Object.entries(setVariables).reduce<VariableCollector>(
    (col, [name, { to, to_after_key_up, to_if_alone, to_if_held_down }]) => {
      if (to !== undefined) col.toVariables.push(toSetVariable([name, to]));
      if (to_after_key_up !== undefined)
        col.toAfterKeyUpVariables.push(toSetVariable([name, to_after_key_up]));
      if (to_if_alone !== undefined)
        col.toIfAloneVariables.push(toSetVariable([name, to_if_alone]));
      if (to_if_held_down !== undefined)
        col.toIfHeldDownVariables.push(toSetVariable([name, to_if_held_down]));
      return col;
    },
    {
      toVariables: [],
      toAfterKeyUpVariables: [],
      toIfAloneVariables: [],
      toIfHeldDownVariables: [],
    },
  );

export const remap = (
  fromTuple: FromKeyCodeTuple,
  toTuples: ToKeyCodeTuple[],
  options: RemapOptions = {},
): Manipulator => {
  const fromKeyText = keyToString(fromTuple);
  const toKeyText = keyToString(...toTuples);
  const { manipulatorOptions, fromOptions, toOptions, setVariables } = options;
  const { toVariables, toAfterKeyUpVariables, toIfAloneVariables, toIfHeldDownVariables } =
    collectVariables(setVariables);

  return {
    type: 'basic',
    from: fromKeyCode(fromTuple, fromOptions),
    to: [...toVariables, ...toTuples.map((toTuple) => toKeyCode(toTuple, toOptions))],
    ...(toAfterKeyUpVariables.length ? { to_after_key_up: toAfterKeyUpVariables } : null),
    ...(toIfAloneVariables.length ? { to_if_alone: toIfAloneVariables } : null),
    ...(toIfHeldDownVariables.length ? { to_if_held_down: toIfHeldDownVariables } : null),
    description: fromKeyText && toKeyText ? `from ${fromKeyText} to ${toKeyText}` : undefined,
    ...manipulatorOptions,
  };
};

export const command = (fromTuple: FromKeyCodeTuple, shell_command: string): Manipulator => {
  const fromKeyText = keyToString(fromTuple);

  return {
    type: 'basic',
    from: fromKeyCode(fromTuple),
    to: [{ shell_command }],
    description: `from ${fromKeyText} to shell command "${shell_command}"`,
  };
};

/**
 * Mod-tap: acts as a modifier when held, sends a keycode when tapped.
 * E.g. a key that sends Escape on tap but acts as Control when held.
 */
export const modTap = (
  fromTuple: FromKeyCodeTuple,
  toTuples: ToKeyCodeTuple[],
  toTuplesOnTap: ToKeyCodeTuple[],
  options: RemapOptions = {},
): Manipulator => {
  const fromKeyText = keyToString(fromTuple);
  const toKeyText = keyToString(...toTuples);
  const toKeyOnTapText = keyToString(...toTuplesOnTap);
  const { manipulatorOptions, toOptions, fromOptions, setVariables } = options;
  const { toVariables, toAfterKeyUpVariables, toIfAloneVariables, toIfHeldDownVariables } =
    collectVariables(setVariables);

  return {
    type: 'basic',
    from: fromKeyCode(fromTuple, fromOptions),
    to: [...toVariables, ...toTuples.map((toTuple) => toKeyCode(toTuple, toOptions))],
    ...(toAfterKeyUpVariables.length ? { to_after_key_up: toAfterKeyUpVariables } : null),
    to_if_alone: [
      ...toIfAloneVariables,
      ...toTuplesOnTap.map((toTupleOnTap) => toKeyCode(toTupleOnTap)),
    ],
    ...(toIfHeldDownVariables.length ? { to_if_held_down: toIfHeldDownVariables } : null),
    description: `${fromKeyText} to ${toKeyText}, send ${toKeyOnTapText} on tap`,
    ...manipulatorOptions,
  };
};
