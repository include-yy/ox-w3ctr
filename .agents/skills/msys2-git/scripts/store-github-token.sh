#!/usr/bin/env bash
# Store the GitHub token from Windows' Git Credential Manager into
# MSYS2's ~/.git-credentials, and set credential.helper = store.
#
# Never prints the token.  The token is kept in plaintext (chmod 600);
# see the SKILL.md for the trade-off.

set -euo pipefail

GITBASH=""
for c in "/c/Program Files/Git/bin/git.exe" "/c/Program Files (x86)/Git/bin/git.exe"; do
  if [ -x "$c" ]; then GITBASH="$c"; break; fi
done
if [ -z "$GITBASH" ]; then
  echo "Git Bash git not found" >&2
  exit 1
fi

cred=$(printf 'protocol=https\nhost=github.com\n\n' | "$GITBASH" credential fill 2>/dev/null)
user=$(printf '%s\n' "$cred" | sed -n 's/^username=//p')
pass=$(printf '%s\n' "$cred" | sed -n 's/^password=//p')

if [ -z "$pass" ]; then
  echo "no GitHub token retrieved from GCM" >&2
  exit 1
fi

printf 'https://%s:%s@github.com\n' "$user" "$pass" > "$HOME/.git-credentials"
chmod 600 "$HOME/.git-credentials"
git config --global credential.helper store
echo "stored GitHub credentials for $user (token not printed)"
