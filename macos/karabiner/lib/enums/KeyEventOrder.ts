const KEY_EVENT_ORDER = ['insensitive' , 'strict' , 'strict_inverse'] as const;

export type KeyEventOrder = typeof KEY_EVENT_ORDER[number];
