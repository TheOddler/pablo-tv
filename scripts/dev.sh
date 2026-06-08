#!/usr/bin/env bash

# This script run both the backend and frontend, and recompiles both on change.
# The backend serves the frontend at its root and support hot-reloading.

set -euo pipefail

run_elm_gen="cabal run elm-gen -f development -- --out=frontend/src"
format_elm_gen="elm-format ./frontend/src/Generated --yes"
run_backend="cabal run pablo-tv:pablo-tv -f development -- --frontend=frontend/static"

backend="watchexec -e hs --restart \"$run_elm_gen && $format_elm_gen && $run_backend\""
frontend="(cd frontend && npx --yes elm-watch hot)"
css="sass --watch frontend/css/main.scss frontend/static/main.css"

parallel --line-buffer \
  --tagstring "{1}" --xapply {2} ::: "🌐" "🌳" "🎨" \
  ::: "$backend" "$frontend" "$css"
