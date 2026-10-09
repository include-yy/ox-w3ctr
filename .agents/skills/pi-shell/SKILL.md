---
name: pi-shell
description: Switch the shell pi's `bash` tool uses (Git Bash vs standalone MSYS2) by editing ~/.pi/agent/settings.json, and restore the default. Use only when the user explicitly asks to switch or restore the shell.
---

# Switching pi's shell

Change which bash executable pi's `bash` tool runs.  This edits the
settings file for the NEXT session; the current session keeps its
shell.  Never switch the shell as a side effect of another task.

## Shells on this machine

| Target | shellPath | shellCommandPrefix |
|---|---|---|
| `git-bash` (default) | remove the key | remove the key |
| `msys2-ucrt64` | `D:\\_D\\msys64\\usr\\bin\\bash.exe` | `export MSYSTEM=UCRT64; export PATH="/ucrt64/bin:/usr/local/bin:/usr/bin:/bin:$PATH"` |
| `msys2-mingw64` | same bash.exe | MSYSTEM=MINGW64, `/mingw64/bin` |
| `msys2-clang64` | same bash.exe | MSYSTEM=CLANG64, `/clang64/bin` |

`git-bash` restores the default: removing both keys makes pi
auto-discover Git Bash under `Program Files`.

## Two homes: MSYS2 vs Windows

The `bash` tool's `$HOME` is not where pi keeps its files.  Three
notions coexist, and they only diverge under standalone MSYS2:

| Notion | Value | Who uses it |
|---|---|---|
| bash `$HOME` | `/home/<user>` (`D:\_D\msys64\home\<user>`) | MSYS2 programs; its own `.bashrc` |
| Windows profile | `C:\Users\<user>` (`/c/Users/<user>`) | pi, node, UCRT64/MINGW64 python |
| Python `expanduser('~')` | always the Windows profile | native-Windows CPython |

Git Bash unifies the two (`$HOME` is `/c/Users/<user>`), which is why
this never bit before.  Standalone MSYS2 sets `$HOME` to its own
virtual home.

Mechanism: the UCRT64/MINGW64 python is a native Windows binary, and
CPython's `expanduser('~')` reads `USERPROFILE`, not `HOME` -- even
though MSYS2 rewrites `HOME` to a Windows path before passing it to
native programs.  So `~` in Python (and in pi, via node) is always
`C:\Users\<user>`, from any shell.

Rule: everything pi-related (settings, sessions) lives in the Windows
profile.  Reach it with `"$USERPROFILE"`, `cygpath -u "$USERPROFILE"`,
or Python's `expanduser('~')` -- never bash's `$HOME`.

## Workflow

1. **Run the switch script** (from any shell):

   ```bash
   python .agents/skills/pi-shell/scripts/switch-shell.py <target>
   ```

   The script backs up to `settings.json.bak`, edits only the two
   shell keys (all other settings are preserved), and validates the
   JSON.

2. **Tell the user to restart pi** (or run `/reload`), then verify:

   ```bash
   echo "$BASH"; cygpath -w "$BASH"; echo "${MSYSTEM:-}"; command -v gcc pacman head
   ```

3. **Refresh `.agents/env.md`.**  The shell fact changed, so re-run the
   env-detect skill (the single writer of env.md) to rewrite it.

4. **Restoring the default** is `switch-shell.py git-bash`.

## Rules

- Only on explicit request.  Never switch or restore as a side effect.
- `shellPath` must be a real executable (`bash.exe`), never
  `msys2_shell.cmd` — that is an interactive `.cmd` launcher, not a
  spawnable shell.
- The MSYS2 prefix must set BOTH parts of the profile's PATH: the
  subsystem bin and `/usr/local/bin:/usr/bin:/bin`.  Omit the base
  part and `ls`, `grep`, `head`, `pacman`, `cygpath` all vanish.
- Pick one subsystem; do not mix `/ucrt64` and `/mingw64` (both have
  their own `gcc`).
- The settings file lives in the Windows profile, not the MSYS2
  `$HOME` -- see "Two homes" above.  The script resolves it correctly
  with `os.path.expanduser`.
- The change takes effect only after a restart; the running session
  keeps its old shell.
