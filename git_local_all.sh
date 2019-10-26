#!/bin/sh

add_repo() {
  if [ -z "${REPO_LIST}" ]; then
    REPO_LIST="$*"
  else
    REPO_LIST="${REPO_LIST} $*"
  fi
}

get_remote() {
  (
    cd "${HOME}/$1"
    git remote -v
  ) | fgrep ssh://192.168.100.200/ | awk '{print $1}' | uniq
}

add_repo dotfiles
add_repo .emacs.d
add_repo .emacs.d/local/ruby-test-unit
add_repo git_work/ansible-target
add_repo git_work/ansible_roles/y10k.kelleyk_emacs
add_repo git_work/ansible_roles/y10k.ruby_build
add_repo git_work/ansible_roles/y10k.rubygems_environment
add_repo git_work/ansible_roles/y10k.rubygems_setup
add_repo git_work/ansible_roles/y10k.rubygems_update
add_repo git_work/imap_proxy
add_repo git_work/imapfetch
add_repo git_work/imapgetall
add_repo git_work/kelleyk_emacs26
add_repo git_work/ldap_example
add_repo git_work/logger-joint
add_repo git_work/memsize
add_repo git_work/raspbian
add_repo git_work/rims
add_repo git_work/rims-passwd-ldap
add_repo git_work/rims-qdbm
add_repo git_work/rims-rfc822
add_repo git_work/riser
add_repo git_work/ruby-build
add_repo git_work/ruby-japanize
add_repo git_work/ruby-qdbm
add_repo git_work/ruby_examples
add_repo git_work/sshd_image
add_repo git_work/sslca_example
add_repo git_work/ubuntu_desktop
add_repo git_work/unix_socket_tunnel
add_repo git_work/windows_desktop

if [ $# -eq 0 ]; then
  echo "$0 GIT_COMMAND..."
  exit 0
fi

error_count=0
for i in ${REPO_LIST}; do
  echo "[$i]"
  remote="$(get_remote "$i")"
  if [ -z "${remote}" ]; then
    echo "warning: not defined remote" >&2
    continue
  fi
  (
    cd "${HOME}/$i"
    git "$@" "${remote}"
  ) || error_count=$((error_count+1))
done

if [ "${error_count}" -gt 0 ]; then
  echo -
  echo "${error_count} errors"
  exit 1
fi
