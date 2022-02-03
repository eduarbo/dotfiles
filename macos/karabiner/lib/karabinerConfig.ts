import type { Profile } from '.';

export interface GlobalSettings {
  check_for_updates_on_startup?: boolean;
  show_in_menu_bar?: boolean;
  show_profile_name_in_menu_bar?: boolean;
}

export interface KarabinerConfig {
  profiles: Profile[];
  global: GlobalSettings;
}
