#!/usr/bin/env bash

# This script runs the tests in a loop, re-testing on change.
# You can --match for a specific test name, or automatically create/reset golden tests with --golden

set -euo pipefail

golden_reset=false
golden_start=false
match=""

while (($#)); do
  case "$1" in
    -g|--golden)
      golden_reset=true
      golden_start=true
      shift
      ;;
    --golden-reset)
      golden_reset=true
      shift
      ;;
    --golden-start)
      golden_start=true
      shift
      ;;
    -m|--match)
      match="$2"
      shift 2
      ;;
    --match=*)
      match="${1#*=}"
      shift
      ;;
    --)
      shift
      break
      ;;
    -*)
      echo "Unknown option: $1" >&2
      exit 2
      ;;
    *) break ;;
  esac
done

opts=""
$golden_reset && opts+=" --golden-reset"
$golden_start && opts+=" --golden-start"
[[ -n $match ]] && opts+=" --match \"$match\""
opts="${opts# }"

test_cmd="cabal test all --test-options=\"$opts\""
watchexec -e hs --restart "$test_cmd"
