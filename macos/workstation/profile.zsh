# Public, deterministic repository ownership for the personal workstation profile.
# Machine-local overrides remain available through the documented environment variables.
: ${WORKSTATION_REPOSITORY_OWNER:=eduarbo}
: ${WORKSTATION_AGENT_CONFIG_GLOBAL_REPOSITORY:=eduarbo/agent-config-global}
: ${WORKSTATION_AGENT_CONFIG_OWNER_REPOSITORY:=eduarbo/agent-config}
: ${WORKSTATION_PERSONAL_OPS_REPOSITORY:=eduarbo/personal-ops}
typeset -gr WORKSTATION_REPOSITORY_OWNER
typeset -gr WORKSTATION_AGENT_CONFIG_GLOBAL_REPOSITORY
typeset -gr WORKSTATION_AGENT_CONFIG_OWNER_REPOSITORY
typeset -gr WORKSTATION_PERSONAL_OPS_REPOSITORY
