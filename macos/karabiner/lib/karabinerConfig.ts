import type { Profile } from './profile.js';

export interface GlobalSettings {
  check_for_updates_on_startup?: boolean;
  show_in_menu_bar?: boolean;
  show_profile_name_in_menu_bar?: boolean;
  indicate_sticky_modifier_keys_state?: boolean;
}

export interface KarabinerConfig {
  profiles: Profile[];
  global: GlobalSettings;
}
