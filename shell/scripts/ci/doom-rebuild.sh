#!@shell@

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

doom sync -u

doom build -r

doom doctor --pager cat
