import { crkbdId } from './enums';
import type { DeviceCondition } from '.';

export const ignoreCrkbd: DeviceCondition = {
  type: 'device_unless',
  identifiers: [
    {
      vendor_id: crkbdId,
    },
  ],
};
