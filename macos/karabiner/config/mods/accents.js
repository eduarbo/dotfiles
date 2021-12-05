const { remap } = require('../../utils');

const mandatoryMods = ['open_bracket'];
const optionalMods = [''];
const remapToLayer = (keyCode, toKey) => remap([keyCode, mandatoryMods, optionalMods], toKey);

module.exports = {
	title: 'Accents layer',
	rules: [
		{
			// eslint-disable-next-line max-len
			description: 'Allow to use the key as modifier when the accent key and the vowel key are pressed simultaneously when typing quickly',
			manipulators: [
				remapToLayer('a', [['e', ['left_option']], ['a']]),
				// FIXME
				// remap([['e', 'open_bracket']], [['e', ['left_option']], ['e']], { simultaneous: true }),
				remapToLayer('e', [['e', ['left_option']], ['e']]),
				remapToLayer('i', [['e', ['left_option']], ['i']]),
				remapToLayer('o', [['e', ['left_option']], ['o']]),
				remapToLayer('u', [['e', ['left_option']], ['u']]),
			],
		},
	],
};
