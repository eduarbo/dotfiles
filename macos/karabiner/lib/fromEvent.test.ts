import { describe, it, expect } from 'vitest';
import { fromKeyCode, fromConsumerKeyCode, fromPointingButton, fromAny } from './fromEvent.js';

describe('fromKeyCode', () => {
  it('creates a simple key_code event', () => {
    expect(fromKeyCode(['a'])).toEqual({ key_code: 'a' });
  });

  it('creates event with mandatory modifiers', () => {
    expect(fromKeyCode(['a', ['control', 'shift']])).toEqual({
      key_code: 'a',
      modifiers: { mandatory: ['control', 'shift'] },
    });
  });

  it('creates event with null mandatory and optional modifiers', () => {
    expect(fromKeyCode(['a', null, ['any']])).toEqual({
      key_code: 'a',
      modifiers: { optional: ['any'] },
    });
  });

  it('creates event with both mandatory and optional modifiers', () => {
    expect(fromKeyCode(['spacebar', ['command'], ['shift']])).toEqual({
      key_code: 'spacebar',
      modifiers: { mandatory: ['command'], optional: ['shift'] },
    });
  });

  it('merges simultaneous options', () => {
    const result = fromKeyCode(['a'], {
      simultaneous: [{ key_code: 'b' }],
      simultaneous_options: { key_up_when: 'any' },
    });
    expect(result).toEqual({
      key_code: 'a',
      simultaneous: [{ key_code: 'b' }],
      simultaneous_options: { key_up_when: 'any' },
    });
  });
});

describe('fromConsumerKeyCode', () => {
  it('creates a consumer key code event', () => {
    expect(fromConsumerKeyCode(['mute'])).toEqual({
      consumer_key_code: 'mute',
    });
  });

  it('creates event with modifiers', () => {
    expect(fromConsumerKeyCode(['volume_increment', ['shift']])).toEqual({
      consumer_key_code: 'volume_increment',
      modifiers: { mandatory: ['shift'] },
    });
  });
});

describe('fromPointingButton', () => {
  it('creates a pointing button event', () => {
    expect(fromPointingButton(['button1'])).toEqual({
      pointing_button: 'button1',
    });
  });

  it('creates event with modifiers', () => {
    expect(fromPointingButton(['button2', ['command'], ['any']])).toEqual({
      pointing_button: 'button2',
      modifiers: { mandatory: ['command'], optional: ['any'] },
    });
  });
});

describe('fromAny', () => {
  it('creates an any event', () => {
    expect(fromAny(['key_code'])).toEqual({ any: 'key_code' });
  });

  it('creates event with modifiers', () => {
    expect(fromAny(['pointing_button', ['shift']])).toEqual({
      any: 'pointing_button',
      modifiers: { mandatory: ['shift'] },
    });
  });
});
