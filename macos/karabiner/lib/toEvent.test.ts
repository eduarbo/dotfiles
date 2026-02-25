import { describe, it, expect } from 'vitest';
import {
  toKeyCode,
  toConsumerKeyCode,
  toPointingButton,
  toShellCommand,
  toSelectInputSource,
  toSetVariable,
  toMouseKey,
  toStickyModifier,
  toHyperKeyCodeTuple,
  toMehKeyCodeTuple,
  toSuperKeyCodeTuple,
} from './toEvent.js';

describe('toKeyCode', () => {
  it('creates a simple key_code event', () => {
    expect(toKeyCode(['a'])).toEqual({ key_code: 'a', modifiers: undefined });
  });

  it('creates event with modifiers', () => {
    expect(toKeyCode(['a', ['command', 'shift']])).toEqual({
      key_code: 'a',
      modifiers: ['command', 'shift'],
    });
  });

  it('merges common options', () => {
    expect(toKeyCode(['escape'], { lazy: true, repeat: false })).toEqual({
      key_code: 'escape',
      modifiers: undefined,
      lazy: true,
      repeat: false,
    });
  });
});

describe('toConsumerKeyCode', () => {
  it('creates a consumer key code event', () => {
    expect(toConsumerKeyCode(['mute'])).toEqual({
      consumer_key_code: 'mute',
      modifiers: undefined,
    });
  });
});

describe('toPointingButton', () => {
  it('creates a pointing button event', () => {
    expect(toPointingButton(['button1'])).toEqual({
      pointing_button: 'button1',
      modifiers: undefined,
    });
  });
});

describe('toShellCommand', () => {
  it('creates a shell command event', () => {
    expect(toShellCommand('open -a Safari')).toEqual({
      shell_command: 'open -a Safari',
    });
  });

  it('merges common options', () => {
    expect(toShellCommand('echo hi', { halt: true })).toEqual({
      shell_command: 'echo hi',
      halt: true,
    });
  });
});

describe('toSelectInputSource', () => {
  it('creates a select input source event', () => {
    expect(toSelectInputSource({ language: 'en' })).toEqual({
      select_input_source: { language: 'en' },
    });
  });
});

describe('toSetVariable', () => {
  it('creates a set variable event', () => {
    expect(toSetVariable(['my_mode', 1])).toEqual({
      set_variable: { name: 'my_mode', value: 1 },
    });
  });

  it('handles boolean values', () => {
    expect(toSetVariable(['active', true])).toEqual({
      set_variable: { name: 'active', value: true },
    });
  });

  it('handles string values', () => {
    expect(toSetVariable(['state', 'on'])).toEqual({
      set_variable: { name: 'state', value: 'on' },
    });
  });
});

describe('toMouseKey', () => {
  it('creates a mouse key event', () => {
    expect(toMouseKey({ x: 100, y: 0 })).toEqual({
      mouse_key: { x: 100, y: 0 },
    });
  });
});

describe('toStickyModifier', () => {
  it('creates a sticky modifier event', () => {
    expect(toStickyModifier({ command: 'toggle' })).toEqual({
      sticky_modifier: { command: 'toggle' },
    });
  });
});

describe('preset tuples', () => {
  it('defines hyper as right_shift + option + command + control', () => {
    expect(toHyperKeyCodeTuple).toEqual(['right_shift', ['option', 'command', 'control']]);
  });

  it('defines meh as right_shift + option + control', () => {
    expect(toMehKeyCodeTuple).toEqual(['right_shift', ['option', 'control']]);
  });

  it('defines super as right_command + option + control', () => {
    expect(toSuperKeyCodeTuple).toEqual(['right_command', ['option', 'control']]);
  });
});
