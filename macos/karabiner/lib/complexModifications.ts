import type { Manipulator } from './manipulator';

export interface Rule {
  description: string;
  manipulators: Manipulator[];
}

export interface ComplexModifications {
  title: string;
  rules: Rule[];
}

export const complexModifications = (title: string, rules: Rule[]): ComplexModifications => ({
  title,
  rules,
});
