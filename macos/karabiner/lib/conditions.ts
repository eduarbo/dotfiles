import { crkbd } from './devices';
import type { DeviceCondition } from '.';

export const ignoreCrkbd: DeviceCondition = {
  type: 'device_unless',
  identifiers: [
    {
      vendor_id: crkbd.vendor_id,
    },
  ],
};
