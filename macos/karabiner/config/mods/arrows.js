const mapKey = (fromKey, toKey) => ({
	type: 'basic',
	from: {
		key_code: fromKey,
		modifiers: {
			mandatory: ['right_option'],
			optional: ['any'],
		},
	},
	to: [{ key_code: toKey }],
});

module.exports = {
	title: 'Arrow keys',
	rules: [{
		description: 'Option + H/J/K/L to arrow keys',
		manipulators: [
			mapKey('h', 'left_arrow'),
			mapKey('j', 'down_arrow'),
			mapKey('k', 'up_arrow'),
			mapKey('l', 'right_arrow'),
		],
	}],
};
