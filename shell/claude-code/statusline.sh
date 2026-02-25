#!/usr/bin/env zsh
# Status line para Claude Code - compatible con Powerlevel10k
# Colores de 8 colores para mantener consistencia con p10k

# Lee el JSON de stdin
input=$(cat)

# Extrae información del JSON usando jq
MODEL=$(echo "$input" | jq -r '.model.display_name // "unknown"')
DIR=$(echo "$input" | jq -r '.workspace.current_dir // "~"')
PCT=$(echo "$input" | jq -r '.context_window.used_percentage // 0' | cut -d. -f1)
COST=$(echo "$input" | jq -r '.cost.total_cost_usd // 0')

# Extrae solo el nombre del directorio actual
DIR_NAME="${DIR##*/}"
[[ -z "$DIR_NAME" ]] && DIR_NAME="~"

# Determina el color del contexto según el porcentaje usado
# Cyan (6) si < 60%, Yellow (3) si < 80%, Red (1) si >= 80%
if [[ $PCT -lt 60 ]]; then
    CTX_COLOR="%6F"  # cyan
elif [[ $PCT -lt 80 ]]; then
    CTX_COLOR="%3F"  # yellow
else
    CTX_COLOR="%1F"  # red
fi

# Construye la barra de progreso (10 caracteres)
FILLED=$((PCT / 10))
EMPTY=$((10 - FILLED))
BAR=""
for ((i=0; i<FILLED; i++)); do BAR+="━"; done
for ((i=0; i<EMPTY; i++)); do BAR+="─"; done

# Formatea el costo con 2 decimales
COST_FORMATTED=$(printf "%.2f" $COST)

# Output con formato similar a p10k (usando los mismos colores)
# %F = color foreground, %f = reset color
printf "%s%s %s%s %s%s %s%s %s%s" \
    "%8F" "in" \
    "%4F" "󱚟 $MODEL" \
    "%8F" "at" \
    "%12F" "📁 $DIR_NAME" \
    "$CTX_COLOR" "[$BAR $PCT%%] " \
    "%8F" "\$$COST_FORMATTED"
