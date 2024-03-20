# KEGen

This is my Karabiner-Elements Configuration Generator in an attempt to manage my configuration easily.

## Export **complex** *modifications*

This *is* a **test**

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
## Frequently Asked Questions (FAQ)
### How does **Karabiner-Elements** handle complex modifications precedence?
Karabiner-Elements operates on a "first match, first served" basis. During scanning, it applies the first matching modification it encounters and ceases further search. To prioritize a particular rule, position it at the top of your list in the configuration. You can rearrange rules via drag-and-drop in the GUI, or by editing the JSON file (higher positioned = higher priority). Beware that overlapping rules can cause unforeseen behavior - strive for mutually exclusive criteria in your rules where feasible.

### Why this distribuition?

- Super Mod: Shortcuts más usados. En la mano izquierda están los que suelen usarse conjunto con el mouse, ya que queda libre mi mano derecha para eso
- Shift: En el lado izquierdo para poder usarlo en conjunto con los modificadores que se encuentran del lado contrario y con el mouse
- Symbols Layer: For all those symbols that didn't fit on the base layer. Positioned in the right side to be able to use the arrow keys (hjkl) with one hand
- Command: en ambos lados ya que como la tecla shift suele usarse muy a menudo en conjuto con otros modificadores, además de que por consistencia con el teclado integrado de la computadora las dos teclas equivaldrían a la barra espaciadora que es una sola tecla posicionada en el centro del teclado que puede usarse en conjunto con cualquier modificador que se encuentre a sus lados.
- Alt: Este modificador es el menos usado en macOS, por lo cual preferí cederle el lugar del thumbs cluster a la tecla Super
- Quiero que el espacio esté lo más accesible posible, y ese lugar es en la tecla shift o symbols que son los modificadores más usados al escribir.
- Enter: Está en abmos lados izquierdo ya que copiar, pegar y presionar enter mientras uso el mouse se ha vuelto una práctica común. Al estar del lado derecho tengo que mover mi mano del mouse al teclado para presionar enter y luego regresar al mouse.
- Sticky mods (CMD, CTRL, ALT) disponibles al mantener presionado Super para las raras ocasiones que necesite combinar mods
- Evitar poner Enter en la tecla Shift o Symbols para evitar presionarla por error mientras escribo

<!-- [![Keyboard Layout Editor](keyboard-layout.png)](http://www.keyboard-layout-editor.com/#/gists/3b8aa95c07c388e37f393d3b0e141f25) -->

### Symbols chart

#### Modifiers

| Symbol | description   | Symbol | description    |
|--------|---------------|--------|----------------|
| `⌘`    | command       | `⌥`    | option         |
| `⌃`    | control       | `⇧`    | shift          |
| `𝕊`    | super (`⌘⌥⌃`) | `𝕙`    | hyper (`⇧⌘⌥⌃`) |

#### Layers

| Symbol | description | Symbol | description |
|--------|-------------|--------|-------------|
| `⇪`    | symbols     | `ƒ`    | f-keys      |
| `≡`    | adjust      |        |             |

#### Other Keys

| Symbol    | description       | Symbol    | description |
|-----------|-------------------|-----------|-------------|
| `␣`       | space             | `⇥`       | tab         |
| `⌫`       | backspace         | `⌦`       | delete      |
| `⏎`       | enter             | `⎋`       | escape      |
| `▲`       | page up           | `▼`       | page down   |
| `⇞`       | home              | `⇟`       | end         |
| `⇧` + `´` | CAPS_WORD         |           |             |

BASE: Standard QWERTY layout, with the delete key replacing the slash key, modifiers set as mod-tap, and central modifiers treated as one. This allows replication of the layout on a MacBook keyboard using the space key in place of the central modifiers.
WINUX: Swaps Ctrl and Gui keys for non-Mac systems
SYMBOLS: Left-side numpad, right-side symbols and arrows, with `,`, `.`, and `⌫` retained for easy number typing
GAMEPAD: Right-shifted QWERTY for FPS games using ESDF as arrow keys
SUPER: Left-side shortcuts for one-handed use, ideal for multitasking with mouse/trackpad or eating Cheetos
FKEYS: Function keys on the left, Media keys on the right

### Base

Standard QWERTY layout, with the delete key replacing the slash key, modifiers set as mod-tap, and central modifiers treated as one. This allows replication of the layout on a MacBook keyboard using the space key in place of the central modifiers.

```text
┌───┬───┬───┬───┬───┐       ┌───┬───┬───┬───┬───┐
│ Q │ W │ E │ R │ T │       │ Y │ U │ I │ O │ P │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│ A │ S │ D │ F │ G │       │ H │ J │ K │ L │ ; │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│ Z │ X │ C │ V │ B │       │ N │ M │ , │ . │ ⌫ │
└───┴───┴──┬┴──┬┴──┬┴──┐ ┌──┴┬──┴┬──┴┬──┴───┴───┘
           │ ⎋ │ ␣ │ ⏎ │ │ ⏎ │ ⇥ │F13│ ⮜ on tap
           │ § │ ⇧ │ ⌘ │ │ ⌘ │ ⇪ │ ⌃ │ ⮜ on hold
           └───┴───┴───┘ └───┴───┴───┘
```

### Winux

Swaps Ctrl and Gui keys for non-Mac systems.

```text
┌───┬───┬───┬───┬───┐       ┌───┬───┬───┬───┬───┐
│   │   │   │   │   │       │   │   │   │   │   │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│   │   │   │   │   │       │   │   │   │   │   │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│   │   │   │   │   │       │   │   │   │   │   │
└───┴───┴──┬┴──┬┴──┬┴──┐ ┌──┴┬──┴┬──┴┬──┴───┴───┘
           │   │   │ ⏎ │ │ ⏎ │   │F13│ ⮜ on tap
           │   │   │ ⌃ │ │ ⌃ │   │ ⌥ │ ⮜ on hold
           └───┴───┴───┘ └───┴───┴───┘
```

### Symbols

```text
┌───┬───┬───┬───┬───┐       ┌───┬───┬───┬───┬───┐
│ ⇞ │ 7 │ 8 │ 9 │ 0 │       │ \ │ [ │ ] │ / │ ▲ │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│ ⇟ │ 4 │ 5 │ 6 │ = │       │ ← │ ↓ │ ↑ │ → │ ▼ │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│ ´ │ 1 │ 2 │ 3 │ - │       │ ` │ ' │   │   │   │
└───┴───┴──┬┴──┬┴──┬┴──┐ ┌──┴┬──┴┬──┴┬──┴───┴───┘
           │⇧+⎋│⇧+␣│⇧+⏎│ │   │   │   │ ⮜ on tap
           │ 𝕙 │ ⇧ │ ⌘ │ │   │   │   │ ⮜ on hold
           └───┴───┴───┘ └───┴───┴───┘
```

| Symbol | description | Symbol      | description | Symbol      | description |
|--------|-------------|-------------|-------------|-------------|-------------|
| `F13`  | Alfred      | `⇧` + `F13` | Emojis      | `⌘` + `F13` | Spotlight   |

### Super

```text
┌───┬───┬───┬───┬───┐       ┌───┬───┬───┬───┬───┐
│⇧⌘3│ F8│⌥⌘I│⇧⌘C│MIC│       │SND│1PW│SNP│CLP│UAC│
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│⇧⌘4│⇧⌘[│⌘[ │⌘] │⇧⌘]│       │W←½│WTG│WGR│W→½│LCK│
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│⇧⌘5│⌃↓ │⌘` │⌘⇥ │⌃↑ │       │EMJ│GPT│ ✗ │ ✗ │ ✗ │
└───┴───┴──┬┴──┬┴──┬┴──┐ ┌──┴┬──┴┬──┴┬──┴───┴───┘
           │   │   │   │ │ ⌘ │ ⌥ │ ⌃ │ ⮜ on tap (sticky)
           │   │   │   │ │ ≡ │ 𝕙 │ ƒ │ ⮜ on hold
           └───┴───┴───┘ └───┴───┴───┘
```

#### Right side

**Top Row**
SND: Change Sound output
1PW: 1Password
SNP: Alfred Snippets
CLP: Alfred Clipboard History
UAC: Alfred Universal Actions

**Home Row (HammerSpoon bindings)**
W←½: moves window to the left half of the screen
WTG: moves window to the left third of the screen
WGR: moves window to the right third of the screen
W→½: moves window to the right half of the screen
LCK: Lock screen

**Bottom Row**
EMJ: Alfred Emojis
GPT: MacGPT

#### Left side

MIC: Toggle Mic

##### Screenshots

| <kbd>⇧⌘3</kbd> | <kbd>⇧⌘4</kbd>    | <kbd>⇧⌘5</kbd> |
| -------------- | ----------------- | -------------- |
| entire screen  | section or window | record screen  |

##### Debugger

| <kbd>F8</kbd>       | <kbd>⌥⌘I</kbd>  | <kbd>⇧⌘C</kbd>  |
| ------------------- | --------------- | --------------- |
| pause/resume script | toggle DevTools | inspect element |

##### Browser navigation

| <kbd>⇧⌘[</kbd> | <kbd>⌘[</kbd> | <kbd>⌘]</kbd> | <kbd>⇧⌘]</kbd> |
| -------------- | ------------- | ------------- | -------------- |
| previous tab   | back          | forward       | next tab       |

##### App navigation

| <kbd>⌃↓</kbd> | <kbd>⌘`</kbd>                              | <kbd>⌘⇥</kbd>   | <kbd>⌃↑</kbd>   |
| ------------- | ------------------------------------------ | --------------- | --------------- |
| app windows   | switch window app<br />switch previous app | switch next app | mission control |

### F-Keys

```text
┌───┬───┬───┬───┬───┐       ┌───┬───┬───┬───┬───┐
│F12│F7 │F8 │F9 │PRT│       │F12│F7 │F8 │F9 │PRT│
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│F11│F4 │F5 │F6 │LCK│       │F11│F4 │F5 │F6 │LCK│
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│F10│F1 │F2 │F3 │PAU│       │F10│F1 │F2 │F3 │PAU│
└───┴───┴──┬┴──┬┴──┬┴──┐ ┌──┴┬──┴┬──┴┬──┴───┴───┘
           │   │ ⇧ │   │ │   │ ⇧ │   │
           └───┴───┴───┘ └───┴───┴───┘
```

### Gaming

```text
┌───┬───┬───┬───┬───┐       ┌───┬───┬───┬───┬───┐
│ESC│ W │ ↑ │ R │ T │       │   │   │   │   │   │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│ a │ ← │ ↓ │ → │ G │       │   │   │   │   │   │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│ z │ X │ C │ V │ B │       │   │   │   │   │   │
└───┴───┴──┬┴──┬┴──┬┴──┐ ┌──┴┬──┴┬──┴┬──┴───┴───┘
           │ ⌥ │ ␣ │ ⌃ │ │   │   │   │
           └───┴───┴───┘ └───┴───┴───┘
```

### Adjust

```text
┌───┬───┬───┬───┬───┐       ┌───┬───┬───┬───┬───┐
│TGL│DBG│RBT│EEC│BOT│       │   │   │   │   │   │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│   │HUI│SAI│VAI│SPI│       │   │WIN│GAM│   │   │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│MOD│HUD│SAD│VAD│SPD│       │   │   │   │   │   │
└───┴───┴──┬┴──┬┴──┬┴──┐ ┌──┴┬──┴┬──┴┬──┴───┴───┘
           │   │   │   │ │   │   │   │
           └───┴───┴───┘ └───┴───┴───┘
```

### Templates

```text
┌───┬───┬───┬───┬───┐       ┌───┬───┬───┬───┬───┐
│   │   │   │   │   │       │   │   │   │   │   │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│   │   │   │   │   │       │   │   │   │   │   │
├───┼───┼───┼───┼───┤       ├───┼───┼───┼───┼───┤
│   │   │   │   │   │       │   │   │   │   │   │
└───┴───┴──┬┴──┬┴──┬┴──┐ ┌──┴┬──┴┬──┴┬──┴───┴───┘
           │   │   │   │ │   │   │   │
           └───┴───┴───┘ └───┴───┴───┘
```
