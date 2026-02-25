import { describe, it, expect } from 'vitest';
import { manipulator, remap, modTap, command } from './manipulator.js';

describe('manipulator', () => {
  it('creates a basic manipulator from a FromEvent object', () => {
    const result = manipulator({ key_code: 'a' }, { to: [{ key_code: 'b' }] });
    expect(result).toEqual({
      type: 'basic',
      from: { key_code: 'a' },
      to: [{ key_code: 'b' }],
    });
  });

  it('creates a manipulator from a FromKeyCodeTuple', () => {
    const result = manipulator(['a', ['control']], { to: [{ key_code: 'b' }] });
    expect(result).toEqual({
      type: 'basic',
      from: { key_code: 'a', modifiers: { mandatory: ['control'] } },
      to: [{ key_code: 'b' }],
    });
  });

  it('passes through conditions', () => {
    const cond = {
      type: 'frontmost_application_if' as const,
      bundle_identifiers: ['^com.test$'],
    };
    const result = manipulator(['a'], { conditions: [cond] });
    expect(result.conditions).toEqual([cond]);
  });
});

describe('remap', () => {
  it('creates a basic remap', () => {
    const result = remap(['a'], [['b']]);
    expect(result.type).toBe('basic');
    expect(result.from).toEqual({ key_code: 'a' });
    expect(result.to).toEqual([{ key_code: 'b', modifiers: undefined }]);
  });

  it('generates a description', () => {
    const result = remap(['a', ['control']], [['b', ['shift']]]);
    expect(result.description).toBe('from control+a to shift+b');
  });

  it('remaps with multiple to targets', () => {
    const result = remap(['a'], [['b'], ['c']]);
    expect(result.to).toHaveLength(2);
  });

  it('passes from options', () => {
    const result = remap(['a'], [['b']], {
      fromOptions: {
        modifiers: { optional: ['any'] },
      },
    });
    expect(result.from).toEqual({
      key_code: 'a',
      modifiers: { optional: ['any'] },
    });
  });

  it('passes to options (e.g. lazy)', () => {
    const result = remap(['a'], [['b']], { toOptions: { lazy: true } });
    expect(result.to).toEqual([{ key_code: 'b', modifiers: undefined, lazy: true }]);
  });

  it('applies manipulator options override', () => {
    const result = remap(['a'], [['b']], {
      manipulatorOptions: { description: 'custom desc' },
    });
    expect(result.description).toBe('custom desc');
  });

  it('handles setVariables with to', () => {
    const result = remap(['a'], [['b']], {
      setVariables: {
        my_layer: { to: 1 },
      },
    });
    expect(result.to).toEqual([
      { set_variable: { name: 'my_layer', value: 1 } },
      { key_code: 'b', modifiers: undefined },
    ]);
  });

  it('handles setVariables with to_after_key_up', () => {
    const result = remap(['a'], [['b']], {
      setVariables: {
        my_layer: { to: 1, to_after_key_up: 0 },
      },
    });
    expect(result.to_after_key_up).toEqual([
      { set_variable: { name: 'my_layer', value: 0 } },
    ]);
  });

  it('handles setVariables with to_if_alone', () => {
    const result = remap(['a'], [['b']], {
      setVariables: {
        mode: { to_if_alone: 'off' },
      },
    });
    expect(result.to_if_alone).toEqual([
      { set_variable: { name: 'mode', value: 'off' } },
    ]);
  });

  it('handles setVariables with to_if_held_down', () => {
    const result = remap(['a'], [['b']], {
      setVariables: {
        mode: { to_if_held_down: true },
      },
    });
    expect(result.to_if_held_down).toEqual([
      { set_variable: { name: 'mode', value: true } },
    ]);
  });
});

describe('command', () => {
  it('creates a shell command manipulator', () => {
    const result = command(['f5'], 'open -a Safari');
    expect(result).toEqual({
      type: 'basic',
      from: { key_code: 'f5' },
      to: [{ shell_command: 'open -a Safari' }],
      description: 'from f5 to shell command "open -a Safari"',
    });
  });

  it('includes modifier text in description', () => {
    const result = command(['a', ['control']], 'echo hi');
    expect(result.description).toBe('from control+a to shell command "echo hi"');
  });
});

describe('modTap', () => {
  it('creates a mod-tap manipulator', () => {
    const result = modTap(['spacebar'], [['left_shift']], [['return_or_enter']]);
    expect(result.type).toBe('basic');
    expect(result.from).toEqual({ key_code: 'spacebar' });
    expect(result.to).toEqual([{ key_code: 'left_shift', modifiers: undefined }]);
    expect(result.to_if_alone).toEqual([{ key_code: 'return_or_enter', modifiers: undefined }]);
  });

  it('generates correct description', () => {
    const result = modTap(['left_command'], [['left_shift']], [['spacebar']]);
    expect(result.description).toBe('left_command to left_shift, send spacebar on tap');
  });

  it('handles modifiers on from', () => {
    const result = modTap(
      ['left_option', null, ['any']],
      [['left_option', ['left_command', 'left_control']]],
      [['escape']],
    );
    expect(result.from).toEqual({
      key_code: 'left_option',
      modifiers: { optional: ['any'] },
    });
  });

  it('handles setVariables', () => {
    const result = modTap(['a'], [['left_shift']], [['escape']], {
      setVariables: { layer: { to: 1, to_after_key_up: 0 } },
    });
    expect(result.to?.[0]).toEqual({ set_variable: { name: 'layer', value: 1 } });
    expect(result.to_after_key_up).toEqual([
      { set_variable: { name: 'layer', value: 0 } },
    ]);
  });

  it('merges to options like lazy', () => {
    const result = modTap(['a'], [['left_shift']], [['escape']], {
      toOptions: { lazy: true },
    });
    expect(result.to).toEqual([{ key_code: 'left_shift', modifiers: undefined, lazy: true }]);
  });
});
