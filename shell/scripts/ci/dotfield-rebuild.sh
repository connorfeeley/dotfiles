#!@shell@

# Debug:
# set -o xtrace

# ############################################################ #
#   ____  _          _ _    ___        _   _                   #
#  / ___|| |__   ___| | |  / _ \ _ __ | |_(_) ___  _ __  ___   #
#  \___ \| '_ \ / _ \ | | | | | | '_ \| __| |/ _ \| '_ \/ __|  #
#   ___) | | | |  __/ | | | |_| | |_) | |_| | (_) | | | \__ \  #
#  |____/|_| |_|\___|_|_|  \___/| .__/ \__|_|\___/|_| |_|___/  #
#                               |_|                            #
# ############################################################ #

set -o pipefail  # trace ERR through pipes
set -o nounset   # set -u : exit the script if you try to use an uninitialised variable
set -o errexit   # set -e : exit the script if any statement returns a non-true return value

export PATH=@path@:$PATH

PLATFORM=""
HOSTNAME="$(hostname)"
ACTION="${1:-}"

case "$(uname)" in
  Darwin)
    PLATFORM="darwin";;
  Linux)
    PLATFORM="nixos";;
  *)
    # Always use sensible defaults.
    PLATFORM="apolloGuidanceComputer";;
esac

# 'sudo nixos-rebuild' on Linux, 'darwin-rebuild' on MacOS
SUDO=""
[ "${PLATFORM}" != "nixos" ] || SUDO="sudo"

# Pretty-build flake configuration
@nom@ build --no-link "${DOTFIELD_DIR}#${PLATFORM}Configurations.${HOSTNAME}.config.system.build.toplevel"

# Forward first argument to {nixos,darwin}-rebuild (build, switch, etc)
${SUDO} ${PLATFORM}-rebuild --flake "${DOTFIELD_DIR}#${HOSTNAME}" "${ACTION}"
