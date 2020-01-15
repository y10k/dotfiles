#!/bin/sh

get_count() {
  count="$#"
}

get_last_word() {
  while [ $# -gt 1 ]; do
    shift
  done
  word="$1"
}

if [ "$#" -lt 2 ]; then
  echo "usage: $0 [<revision range>] [apply command]..."
  exit 1
fi

commits="$(git log --format=%H --reverse "$1")" || exit 1
shift

get_count ${commits}
if [ "${count}" -gt 30 ]; then
  echo -n "too many ${count} commits. continue? [y/n]: "
  read answer
  case "${answer}" in
    [Yy])
      # continue
    ;;
    *)
      exit 1
    ;;
  esac
fi

for c in ${commits}; do
  git checkout "${c}"
  echo "$@"
  "$@" || exit "$?"
done

get_last_word ${commits}
last_commit="${word}"
last_name="$(git name-rev "${last_commit}" | awk '{print $2}')"
git checkout "${last_name}"
