import { remap, lazyModTap, complexModifications } from '../../lib';

export const main = complexModifications('Main layer', [
  {
    description: 'Thumb cluster',
    manipulators: [
      // NOTE Do NOT move it! order matters: The more specific the binding, the sooner it should be declared
      // Move Control to the right side to be able to combine it with Shift when both are pressed at the same time
      // L Command + R Option -> L Shift + L Control
      lazyModTap(['left_command', ['right_option'], ['any']], [['left_shift', ['left_control']]], [['spacebar']]),

      // Spacebar -> L Command | Return
      lazyModTap(['spacebar', null, ['any']], [['left_command']], [['return_or_enter']]),

      // L Command -> L Shift | Spacebar
      lazyModTap(['left_command', null, ['any']], [['left_shift']], [['spacebar']]),

      // R Command -> R Shift | Escape
      lazyModTap(['right_command', null, ['any']], [['right_shift']], [['escape']]),

      // L Option -> L Control | '
      lazyModTap(['left_option', null, ['any']], [['left_control']], [['quote']]),

      // R Option -> L Control | `
      lazyModTap(['right_option', null, ['any']], [['right_option']], [['grave_accent_and_tilde']]),
    ],
  },
  {
    description: 'Custom Qwerty',
    manipulators: [
      // [ -> '
      remap(['open_bracket', null, ['any']], [['e', ['left_option']]]),

      // ' -> ]
      remap(['quote', null, ['any']], [['close_bracket']]),

      // CAPS -> [
      remap(['caps_lock', null, ['any']], [['open_bracket']]),

      // L Shfit -> Delete
      remap(['left_shift', null, ['any']], [['delete_or_backspace']]),

      // R Shift -> \
      remap(['right_shift', null, ['any']], [['backslash']]),
    ],
  },
]);
