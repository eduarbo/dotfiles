# KEGen

This is my Karabiner-Elements Configuration Generator in an attempt to manage my configuration easily.

## Export complex modifications

```javascript
./complex_modifications.js > ~/.config/karabiner/karabiner.json
```

## Export karabiner.json

```bash
for mod in ./complex_modifications/*
  do ./complex_modifications/$mod > ~/.config/karabiner/assets/complex_modifications/$mod
done
```

## My layout

[[file:keyboard-layout.png]]

[Keyboard Layout Editor](http://www.keyboard-layout-editor.com/#/gists/3b8aa95c07c388e37f393d3b0e141f25)
