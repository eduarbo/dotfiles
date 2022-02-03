import type {
  Modifier,
  KeyCode,
  PointingButton,
  ConsumerKeyCode,
  Any,
  KeyEventOrder,
  KeyUpWhen,
} from './enums';
import type { ToEvent } from './toEvent';

type FromKeyCodeProp = { key_code: KeyCode };
type FromConsumerKeyCodeProp = { consumer_key_code: ConsumerKeyCode };
type FromPointingButtonProp = { pointing_button: PointingButton };
type FromAnyProp = { any: Any };

type ModifiersProps = {
  modifiers?: {
    mandatory?: Modifier[];
    optional?: Modifier[];
  };
};

type SimultaneousProps = {
  simultaneous?: (
    | FromKeyCodeProp
    | FromConsumerKeyCodeProp
    | FromPointingButtonProp
    | FromAnyProp
  )[];
  simultaneous_options?: {
    detect_key_down_uninterruptedly?: boolean;
    key_down_order?: KeyEventOrder;
    key_up_order?: KeyEventOrder;
    key_up_when?: KeyUpWhen;
    to_after_key_up?: ToEvent[];
  };
};

export type FromEventCommon = SimultaneousProps & ModifiersProps;

export type FromKeyCodeEvent = FromEventCommon & FromKeyCodeProp;
type FromConsumerKeyCodeEvent = FromEventCommon & FromConsumerKeyCodeProp;
type FromPointingButtonEvent = FromEventCommon & FromPointingButtonProp;
type FromAnyEvent = FromEventCommon & FromAnyProp;

export type FromEvent =
  | FromKeyCodeEvent
  | FromConsumerKeyCodeEvent
  | FromPointingButtonEvent
  | FromAnyEvent;

export type FromKeyCodeTuple = [KeyCode, (Modifier[] | null)?, Modifier[]?];

export const fromKeyCode = (
  [key_code, mandatory, optional]: FromKeyCodeTuple,
  options: SimultaneousProps = {},
): FromKeyCodeEvent => ({
  key_code,
  ...((mandatory || optional) && {
    modifiers: {
      ...(mandatory && { mandatory }),
      ...(optional && { optional }),
    },
  }),
  ...options,
});

export const fromConsumerKeyCode = (
  [consumer_key_code, mandatory, optional]: [ConsumerKeyCode, Modifier[]?, Modifier[]?],
  options: SimultaneousProps = {},
): FromConsumerKeyCodeEvent => ({
  consumer_key_code,
  ...((mandatory || optional) && {
    modifiers: {
      ...(mandatory && { mandatory }),
      ...(optional && { optional }),
    },
  }),
  ...options,
});

export const fromPointingButton = (
  [pointing_button, mandatory, optional]: [PointingButton, Modifier[]?, Modifier[]?],
  options: SimultaneousProps = {},
): FromPointingButtonEvent => ({
  pointing_button,
  ...((mandatory || optional) && {
    modifiers: {
      ...(mandatory && { mandatory }),
      ...(optional && { optional }),
    },
  }),
  ...options,
});

export const fromAny = (
  [any, mandatory, optional]: [Any, Modifier[]?, Modifier[]?],
  options: SimultaneousProps = {},
): FromAnyEvent => ({
  any,
  ...((mandatory || optional) && {
    modifiers: {
      ...(mandatory && { mandatory }),
      ...(optional && { optional }),
    },
  }),
  ...options,
});
