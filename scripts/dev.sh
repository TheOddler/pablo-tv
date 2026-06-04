#!/usr/bin/env bash

# This script run both the backend and frontend, and auto-reload both on change

set -euo pipefail

backend_cmd="watchexec -e hs,hamlet,cabal --restart cabal run backend -f development"
frontend_cmd="(cd frontend && elm-live src/Main.elm --dir=static --proxy-prefix=/api --proxy-host=http://localhost:8080 -- --output=static/main.js)"

parallel --line-buffer \
  --tagstring "{1}" --xapply {2} ::: "🌐" "🌳" \
  ::: "$backend_cmd" "$frontend_cmd"
