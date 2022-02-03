const ANY = ['key_code', 'consumer_key_code', 'pointing_button'] as const;

export type Any = typeof ANY[number];
