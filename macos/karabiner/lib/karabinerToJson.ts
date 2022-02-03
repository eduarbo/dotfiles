import type { Profile } from './profile';

interface GlobalSettings {
  check_for_updates_on_startup?: boolean;
  show_in_menu_bar?: boolean;
  show_profile_name_in_menu_bar?: boolean;
}

const JSON_SPACE_INDENTATION = 2;

export const karabinerToJson = (profiles: Profile[], global: GlobalSettings = {}) => {
  const karabiner = {
    profiles,
    global,
  };

  const json = JSON.stringify(karabiner, null, JSON_SPACE_INDENTATION);

  // eslint-disable-next-line no-console
  console.log(json);
};
