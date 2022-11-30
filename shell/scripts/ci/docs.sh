#!@shell@
# shellcheck shell=bash

# Debug:
# set -o xtrace

export PATH=@path@:$PATH

# Generate a personal access token and export it as PRIVATE_BEARER_TOKEN:
ACCESS_TOKEN_FILE=/run/agenix/dotfield-readme-update-access-token
[ -f "${ACCESS_TOKEN_FILE}" ] && PRIVATE_BEARER_TOKEN="$(cat "${ACCESS_TOKEN_FILE}")"

# ############################################################ #
#   ____  _          _ _    ___        _   _                   #
#  / ___|| |__   ___| | |  / _ \ _ __ | |_(_) ___  _ __  ___   #
#  \___ \| '_ \ / _ \ | | | | | | '_ \| __| |/ _ \| '_ \/ __|  #
#   ___) | | | |  __/ | | | |_| | |_) | |_| | (_) | | | \__ \  #
#  |____/|_| |_|\___|_|_|  \___/| .__/ \__|_|\___/|_| |_|___/  #
#                               |_|                            #
# ############################################################ #

set -o pipefail # trace ERR through pipes
set -o nounset  # set -u : exit the script if you try to use an uninitialised variable
set -o errexit  # set -e : exit the script if any statement returns a non-true return value

# ###################### #
#  __  __       _        #
# |  \/  | __ _(_)_ __   #
# | |\/| |/ _` | | '_ \  #
# | |  | | (_| | | | | | #
# |_|  |_|\__,_|_|_| |_| #
# ###################### #

bearer_token="${PRIVATE_BEARER_TOKEN}"

pandoc README.org -f org -t html5 -o README.html

###
### First-time setup
###

# And this with your repository name:
# repo_name=dotfield

# curl --oauth2-bearer $bearer_token \
#   -G --data-urlencode query='query { me { repository(name: "'$repo_name'") { id } } }' \
#   https://git.sr.ht/query


###
### Regular usage
###

# And replace this with your repository ID:
repo_id=171368

# And the readme file:
readme=README.html

# Set the repository's README
jq -sR '{
    "query": "mutation UpdateRepo($id: Int!, $readme: String!) {
      updateRepository(id: $id, input: { readme: $readme }) { id }
    }", "variables": {
      "id": '$repo_id',
      "readme": .
    } }' < $readme \
  | curl --oauth2-bearer $bearer_token \
    -H "Content-Type: application/json" \
    -d@- https://git.sr.ht/query

# Clear the repository's README
# jq -sR '{
#     "query": "mutation UpdateRepo($id: Int!) {
#       updateRepository(id: $id, input: { readme: null }) { id }
#     }", "variables": {
#       "id": '$repo_id'
#     } }' \
#   | curl --oauth2-bearer $bearer_token \
#     -H "Content-Type: application/json" \
#     -d@- https://git.sr.ht/query
