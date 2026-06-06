#!/usr/bin/env bash

# This script run both the backend and frontend, and auto-reload both on change
# The backend serves the frontend at its root, but it doesn't support hot-reloading.
# To have hot-reloading you need to open the page elm-live hosts, it will output the url for that on start.

set -euo pipefail

run_elm_gen="cabal run elm-gen -f development"
format_elm_gen="elm-format ./frontend/src/Generated --yes"
run_backend="cabal run pablo-tv:pablo-tv -f development"

backend="watchexec -e hs --restart \"$run_elm_gen && $format_elm_gen && $run_backend\""
frontend="(cd frontend && elm-live src/Main.elm --hot --dir=static --proxy-prefix=/api --proxy-host=http://localhost:8080 -- --output=static/main.js)"

parallel --line-buffer \
  --tagstring "{1}" --xapply {2} ::: "🌐" "🌳" \
  ::: "$backend" "$frontend"
