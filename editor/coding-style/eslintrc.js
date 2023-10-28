module.exports = {
  root: true,
  env: {
    browser: true,
    commonjs: true,
    node: true,
    es2021: true,
    // jest: true,
  },
  extends: [
    "airbnb", // If you need React
    // 'airbnb-base', // If you don't need React
    // 'plugin:jest/recommended',
    "prettier",
  ],
  plugins: [
    // 'jest',
    "prettier",
  ],
  parserOptions: {
    ecmaVersion: 12,
  },
  rules: {
    "no-use-before-define": ["error", { functions: false }],
    "no-param-reassign": ["error", { props: false }],
    "max-len": ["error", { code: 100 }],
    camelcase: "off",
    // Cannot reassign function parameters but allowing modification
    indent: ["error", 2],
    // allows mixed tabs and spaces when the spaces are used for alignment
    "no-mixed-spaces-and-tabs": ["error", "smart-tabs"],
    // require trailing commas in multiline object literals
    "comma-dangle": [
      "error",
      {
        arrays: "always-multiline",
        objects: "always-multiline",
        imports: "always-multiline",
        exports: "always-multiline",
        functions: "always-multiline",
      },
    ],
  },
};
