#!/bin/bash

if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS
  open _build/default/_doc/_html/index.html/
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
  if [[ -n "$IS_WSL" || -n "$WSL_DISTRO_NAME" ]]; then
    # WSL
    DOCPATH=$(wslpath -w ./_build/default/_doc/_html/index.html)
    explorer.exe ${DOCPATH} || true
  else
    nautilus _build/default/_doc/_html/cube/
  fi
fi
