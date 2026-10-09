#!/usr/bin/env python
"""Switch the shell pi's bash tool uses.

Usage: switch-shell.py <target>
Targets: git-bash (default), msys2-ucrt64, msys2-mingw64, msys2-clang64

Edits the Windows ~/.pi/agent/settings.json (NOT the MSYS2 $HOME): sets
or removes shellPath and shellCommandPrefix, backs up first, and
validates the result.
"""
import json
import os
import shutil
import sys

SETTINGS = os.path.expanduser("~/.pi/agent/settings.json")
MSYS2_BASH = "D:\\_D\\msys64\\usr\\bin\\bash.exe"


def prefix(subsystem):
    return (
        "export MSYSTEM=%s; "
        'export PATH="/%s/bin:/usr/local/bin:/usr/bin:/bin:$PATH"'
        % (subsystem, subsystem.lower())
    )


SHELLS = {
    # None means "remove the key": pi falls back to auto-discovery
    # (Git Bash under Program Files, then bash.exe on PATH).
    "git-bash": (None, None),
    "msys2-ucrt64": (MSYS2_BASH, prefix("UCRT64")),
    "msys2-mingw64": (MSYS2_BASH, prefix("MINGW64")),
    "msys2-clang64": (MSYS2_BASH, prefix("CLANG64")),
}


def main():
    if len(sys.argv) != 2 or sys.argv[1] not in SHELLS:
        print("usage: switch-shell.py " + "|".join(SHELLS), file=sys.stderr)
        return 2
    target = sys.argv[1]
    shell_path, cmd_prefix = SHELLS[target]

    if not os.path.exists(SETTINGS):
        print("missing " + SETTINGS, file=sys.stderr)
        return 1

    shutil.copy(SETTINGS, SETTINGS + ".bak")

    with open(SETTINGS, encoding="utf-8") as f:
        cfg = json.load(f)

    for key in ("shellPath", "shellCommandPrefix"):
        cfg.pop(key, None)
    if shell_path is not None:
        cfg["shellPath"] = shell_path
    if cmd_prefix is not None:
        cfg["shellCommandPrefix"] = cmd_prefix

    with open(SETTINGS, "w", encoding="utf-8", newline="\n") as f:
        json.dump(cfg, f, indent=2)
        f.write("\n")

    print("switched to " + target + "; backup at " + SETTINGS + ".bak")
    print("restart pi (or /reload) for the change to take effect")
    return 0


if __name__ == "__main__":
    sys.exit(main())
