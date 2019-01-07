#!/bin/sh

make_link() {
  local repo_name="$1"
  local el_name="$2"

  if [ -d "${repo_name}" ]; then
    return
  fi

  repo_path="../../../${repo_name}"
  if [ ! -d "${repo_path}" ]; then
    echo "error: not exist local repository: #{mod_path}"
    exit 1
  fi

  ln -s "${repo_path}/${el_name}" "${el_name}" || exit "$?"
}

make_link ruby-test-unit ruby-test-unit.el

# Local Variables:
# indent-tabs-mode: nil
# End:
