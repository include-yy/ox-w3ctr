#!/usr/bin/env bash
# Detect this checkout's machine-specific facts and print KEY=value lines.
# The env-detect SKILL.md turns this output into .agents/env.md.
# A FAIL line means the fact could not be verified: do not guess.

say() { printf '%s\n' "$1"; }
fail() { say "FAIL $1: $2"; }

# --- Emacs: PATH first, then the project's known build ---
emacs_cmd=""
for c in emacs /d/emacs-build/bin/emacs.exe; do
  if command -v "$c" >/dev/null 2>&1 || [ -x "$c" ]; then
    emacs_cmd="$c"; break
  fi
done
if [ -n "$emacs_cmd" ]; then
  v="$("$emacs_cmd" --version 2>/dev/null | head -1)"
  major="$(printf '%s' "$v" | grep -oE '[0-9]+' | head -1)"
  if [ -n "$v" ] && [ -n "$major" ] && [ "$major" -ge 31 ] 2>/dev/null; then
    say "emacs=$emacs_cmd"
    say "emacs_version=$v"
  else
    fail emacs "$emacs_cmd reports '$v' (need >= 31)"
  fi
else
  fail emacs "not found (tried PATH and /d/emacs-build/bin/emacs.exe)"
fi

# --- Python: the first candidate that is a working Python 3 ---
py_cmd=""
for c in python python3; do
  if command -v "$c" >/dev/null 2>&1; then
    v="$("$c" --version 2>&1)"
    if printf '%s' "$v" | grep -q '^Python 3\.'; then
      py_cmd="$c"; py_version="$v"; break
    fi
  fi
done
if [ -n "$py_cmd" ]; then
  say "python=$py_cmd"
  say "python_version=$py_version"
else
  fail python "no usable Python 3 found"
fi
# python3 is separately worth reporting: on Windows it is the Store stub.
if command -v python3 >/dev/null 2>&1; then
  v3="$(python3 --version 2>&1)"
  if ! printf '%s' "$v3" | grep -q '^Python 3\.'; then
    say "python3_broken=store-stub"
  fi
fi

# --- Org sources: a checkout with ox.el ---
org=""
for d in /d/org-mode/lisp "$HOME/org-mode/lisp"; do
  if [ -f "$d/ox.el" ]; then org="$d"; break; fi
done
if [ -n "$org" ]; then
  say "org=$org"
else
  fail org "no checkout with ox.el found"
fi

# --- Shell: the actual bash, not just uname.  Git Bash and standalone
#     MSYS2 both report MINGW64, so uname alone cannot tell them apart. ---
bash_path="$(cygpath -w "${BASH:-/usr/bin/bash}" 2>/dev/null)"
if [ -n "$bash_path" ]; then
  say "bash=$bash_path"
else
  fail bash "cygpath could not resolve the running bash"
fi
if command -v pacman >/dev/null 2>&1; then
  say "shell_flavor=msys2"
else
  say "shell_flavor=git-bash"
fi

# --- Proxy: record what reaches github ---
proxy=""
if [ "$(HTTPS_PROXY=http://127.0.0.1:7890 curl -s -o /dev/null -w '%{http_code}' https://github.com 2>/dev/null)" = "200" ]; then
  proxy="http://127.0.0.1:7890"
fi
if [ -n "$proxy" ]; then
  say "proxy=$proxy"
else
  fail proxy "github unreachable via http://127.0.0.1:7890"
fi

# --- Node ---
node_v="$(node --version 2>/dev/null)"
npm_v="$(npm --version 2>/dev/null)"
if [ -n "$node_v" ]; then
  say "node=$node_v"
else
  fail node "node not found"
fi
if [ -n "$npm_v" ]; then
  say "npm=$npm_v"
else
  fail npm "npm not found"
fi

# --- Remotes ---
git remote -v 2>/dev/null | awk '{print $1, $2}' | sort -u | while read -r name url; do
  say "remote_${name}=${url}"
done
