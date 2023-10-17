#!/usr/bin/env sh

# This is the install script for 'garn'.

set -eu

test_nix_installation () {
  nix --version 1> /dev/null 2> /dev/null
}

install_nix () {
  TMP=$(mktemp -d)
  echo "extra-substituters = https://cache.garnix.io" >> "$TMP/nix-extra-config"
  echo "extra-trusted-public-keys = cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=" >> "$TMP/nix-extra-config"

  curl --proto '=https' --tlsv1.2 -sSfL https://releases.nixos.org/nix/nix-2.17.1/install -o "$TMP/install.sh"
  chmod u+x "$TMP/install.sh"

  "$TMP/install.sh" --nix-extra-conf-file "$TMP/nix-extra-config" --daemon --yes

  set +eu
  # shellcheck source=/dev/null
  . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
  set -eu
}

if test_nix_installation; then
  echo Hooray, nix is already installed:
  nix --version
else
  echo \'garn\' depends on nix, but it seems that you don\'t have a nix installation.
  printf "Should I install nix now? [y/n] "
  read -r SHOULD_INSTALL_NIX
  if test "$SHOULD_INSTALL_NIX" != y; then
    echo Cancelling installation.
    exit 1
  fi
  install_nix
  echo nix is now installed:
  nix --version
fi
test_nix_installation

echo TODO: testing binary cache...
# is there a good way to test whether a binary cache is configured and available?

echo "installing 'garn'..."
nix --extra-experimental-features 'nix-command flakes' profile install -L "github:garnix-io/garn"
echo "testing 'garn' installation..."
garn --help

echo "Success! 'garn' is now installed."
