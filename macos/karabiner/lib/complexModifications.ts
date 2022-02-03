import type { Manipulator } from '.';

export interface Rule {
  description: string;
  manipulators: Manipulator[];
}

export interface ComplexModifications {
  title: string;
  rules: Rule[];
}
