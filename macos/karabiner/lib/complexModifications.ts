import type { Manipulator } from './manipulator.js';

export interface Rule {
  description: string;
  manipulators: Manipulator[];
}

export interface ComplexModifications {
  title: string;
  rules: Rule[];
}
