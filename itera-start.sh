#!/usr/bin/env bash
set -euo pipefail

# Port the itera demo server listens on (demo/deps.edn :itera -> :opts :port).
PORT="${1:-${PORT:-5000}}"
DEMO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/demo" && pwd)"

# Print PIDs of processes listening on $PORT (one per line).
pids_on_port() {
  ss -H -ltnp "sport = :$PORT" 2>/dev/null \
    | grep -oE 'pid=[0-9]+' | cut -d= -f2 | sort -u
}

pids="$(pids_on_port || true)"
if [ -n "$pids" ]; then
  echo "Killing existing process(es) on port $PORT: $pids"
  # shellcheck disable=SC2086
  kill $pids 2>/dev/null || true
  for _ in $(seq 1 50); do
    [ -z "$(pids_on_port)" ] && break
    sleep 0.1
  done
  remaining="$(pids_on_port || true)"
  if [ -n "$remaining" ]; then
    echo "SIGTERM ignored; force killing: $remaining"
    # shellcheck disable=SC2086
    kill -9 $remaining 2>/dev/null || true
  fi
else
  echo "No process listening on port $PORT"
fi

cd "$DEMO_DIR"
exec clj -X:itera