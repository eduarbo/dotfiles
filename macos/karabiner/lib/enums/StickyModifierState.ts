const STICKY_MODIFIER_STATE = ['on', 'off', 'toggle'] as const;

export type StickyModifierState = typeof STICKY_MODIFIER_STATE[number];
