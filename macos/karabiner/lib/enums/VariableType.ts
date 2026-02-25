const VARIABLE_TYPE = ['variable_if', 'variable_unless'] as const;

export type VariableType = (typeof VARIABLE_TYPE)[number];
