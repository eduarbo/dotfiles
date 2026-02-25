#!/usr/bin/env zsh
# Status line para Claude Code - compatible con Powerlevel10k
# Usa códigos ANSI para compatibilidad con stdout

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

# Colores ANSI (compatibles con p10k 8-color theme)
GRAY="\e[90m"      # gris oscuro
BLUE="\e[94m"      # azul brillante
CYAN="\e[96m"      # cyan brillante
YELLOW="\e[93m"    # amarillo brillante
RED="\e[91m"       # rojo brillante
RESET="\e[0m"      # reset color

# Determina el color del contexto según el porcentaje usado
if [[ $PCT -lt 60 ]]; then
    CTX_COLOR="$CYAN"
elif [[ $PCT -lt 80 ]]; then
    CTX_COLOR="$YELLOW"
else
    CTX_COLOR="$RED"
fi

# Construye la barra de progreso (10 caracteres)
FILLED=$((PCT / 10))
EMPTY=$((10 - FILLED))
BAR=""
for ((i=0; i<FILLED; i++)); do BAR+="━"; done
for ((i=0; i<EMPTY; i++)); do BAR+="─"; done

# Formatea el costo con 2 decimales
COST_FORMATTED=$(printf "%.2f" $COST)

# Output con formato similar a p10k
printf "${GRAY}in ${BLUE}󱚟 %s ${GRAY}at ${BLUE}📁 %s ${CTX_COLOR}[%s %d%%] ${GRAY}\$%s${RESET}" \
    "$MODEL" \
    "$DIR_NAME" \
    "$BAR" \
    "$PCT" \
    "$COST_FORMATTED"
