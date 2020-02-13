#!/bin/sh

if [ "$#" -lt 2 ]; then
  echo "usage: $0 [RETRY COUNT] COMMANDs..."
  exit 1
fi

retry_count="$1"
shift

if [ "${retry_count}" -lt 1 ]; then
  echo "error: retry count should be greater than 1."
  exit 1
fi

retry_count="$((retry_count - 1))"
while [ "${retry_count}" -gt 0 ]; do
  "$@" && {
    exit 0
  }
  retry_count="$((retry_count - 1))"
done

exec "$@"
