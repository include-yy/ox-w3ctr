#!/usr/bin/env bash
# Detect this checkout's machine-specific facts and print KEY=value lines.
# The env-detect SKILL.md turns this output into .agents/env.md.
# A FAIL line means the fact could not be verified: do not guess.

say() { printf '%s\n' "$1"; }
fail() { say "FAIL $1: $2"; }

# Read a previously recorded value from .agents/env.md (the table's
# first backtick-quoted cell).  Empty when the file or the fact is absent.
known() {
  grep "^| $1 " .agents/env.md 2>/dev/null | grep -oE '`[^`]+`' | head -1 | tr -d '`'
}

# --- Emacs: EMACS env var, PATH, then the env.md record; a FAIL
#     sends the agent to the user, then a scan ---
emacs_cmd="${EMACS:-}"
if [ -z "$emacs_cmd" ] && command -v emacs >/dev/null 2>&1; then
  emacs_cmd="emacs"
fi
if [ -z "$emacs_cmd" ]; then
  emacs_cmd="$(known Emacs)"
fi
if [ -n "$emacs_cmd" ] && { command -v "$emacs_cmd" >/dev/null 2>&1 || [ -x "$emacs_cmd" ]; }; then
  v="$("$emacs_cmd" --version 2>/dev/null | head -1)"
  major="$(printf '%s' "$v" | grep -oE '[0-9]+' | head -1)"
  if [ -n "$v" ] && [ -n "$major" ] && [ "$major" -ge 31 ] 2>/dev/null; then
    say "emacs=$emacs_cmd"
    say "emacs_version=$v"
  else
    fail emacs "$emacs_cmd reports '$v' (need >= 31)"
  fi
else
  fail emacs "not found (tried EMACS, PATH, and the env.md record)"
fi

# --- Python: the first candidate that is a working Python 3 ---
py_cmd=""
for c in python python3; do
  if command -v "$c" >/dev/null 2>&1; then
    py_version="$("$c" --version 2>&1)"
    if printf '%s' "$py_version" | grep -q '^Python 3\.'; then
      py_cmd="$c"; break
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

# --- Org sources: ORG_SOURCES env var, the env.md record, else the
#     Org bundled with Emacs (share/emacs/<version>/lisp/org) ---
org=""
known_org="$(known 'Org sources')"
if [ -n "$ORG_SOURCES" ] && [ -f "$ORG_SOURCES/ox.el" ]; then
  org="$ORG_SOURCES"
elif [ -n "$known_org" ] && [ -f "$known_org/ox.el" ]; then
  org="$known_org"
elif [ -n "$emacs_cmd" ] && [ -n "$v" ]; then
  emacs_path="$(command -v "$emacs_cmd" 2>/dev/null || printf '%s' "$emacs_cmd")"
  emacs_prefix="$(dirname "$(dirname "$emacs_path")")"
  emacs_ver="$(printf '%s' "$v" | grep -oE '[0-9]+\.[0-9]+(\.[0-9]+)?' | head -1)"
  bundled="$emacs_prefix/share/emacs/$emacs_ver/lisp/org"
  if [ -f "$bundled/ox.el" ]; then
    org="$bundled"
    # The recorded path is gone: flag it so the agent reports rather
    # than silently overwriting the user's choice.
    if [ -n "$known_org" ]; then
      say "org_previous=$known_org"
    fi
  fi
fi
if [ -n "$org" ]; then
  say "org=$org"
else
  fail org "no org with ox.el found (tried ORG_SOURCES, the env.md record, and the Emacs bundle)"
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
  say "shell_flavor=msys2-${MSYSTEM:-unknown}"
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
