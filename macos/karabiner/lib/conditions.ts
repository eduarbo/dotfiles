import { beekeeb, crkbd } from './devices';
import { games, type Conditions } from '.';

export const ignoreCrkbd: Conditions = {
  type: 'device_unless',
  identifiers: [
    {
      vendor_id: crkbd.vendor_id,
    },
  ],
};

export const ignoreBeekeb: Conditions = {
  type: 'device_unless',
  identifiers: [beekeeb],
};

export const ignoreGames: Conditions = {
  type: 'frontmost_application_unless',
  bundle_identifiers: games,
};

export const ignoreKeebs: Conditions[] = [ignoreCrkbd, ignoreBeekeb];
