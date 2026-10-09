---
name: python-env
description: Create and manage Python virtual environments for this checkout (uv primary, stdlib venv fallback), and keep .agents/env.md's Python facts in sync. Use when setting up, changing, or reinstalling the Python environment.
---

# Managing Python environments

Python tooling in this repo (the verify harness's JSON-RPC helpers and
a few `tools/` scripts) runs in an isolated venv.  Prefer `uv`; fall
back to the stdlib `venv` when `uv` is not installed.

## Tool choice

| Tool | Commands | When |
|---|---|---|
| `uv` | `uv venv`, `uv pip install`, `uv pip freeze` | default |
| `venv` + `pip` | `python -m venv`, `pip install -r` | `uv` unavailable |

Install `uv` under MSYS2 with `pacman -S mingw-w64-ucrt-x86_64-uv`
(or `pip install uv`).

## Workflow

1. **Create** the venv at the repo root:

   ```bash
   uv venv .venv            # or: python -m venv .venv
   ```

   The `.venv/` directory is the standard home.  Ensure `.gitignore`
   lists it (add `.venv/` if missing).

2. **Install and pin** dependencies:

   ```bash
   uv pip install jsonrpcserver       # the verify RPC harness's dep
   uv pip freeze > requirements.txt   # pin what is installed
   ```

3. **Use** the venv per command, not globally.  Prepend its bin:

   ```bash
   export PATH="$(pwd)/.venv/bin:$PATH"
   python --version                   # now the venv's python
   ```

   Do not put this in pi's `shellCommandPrefix`: the default `python`
   must stay the system interpreter, and a venv is opt-in per task.

4. **Record the venv in project files, not env.md.**  The venv is a
   project fact, not a machine fact.  Step 2 already pins the deps in
   `requirements.txt`; note the venv in AGENTS.md only if the repo's
   tooling needs it.  Re-run the env-detect skill only when the
   *system* python itself changed (new interpreter, new subsystem).

## Rules

- MSYS2's python is a native Windows binary, so `python -m venv` may
  lay out a Windows-style venv (`Scripts/`, not `bin/`).  After
  creating, verify where the venv's python landed and use that path;
  do not assume a POSIX layout.
- Never commit `.venv/`; it is local state.  Add `.venv/` to
  `.gitignore` when you create it.
- The venv is a project fact; env.md holds machine facts only.  Pin
  venv deps in `requirements.txt` and note the venv in AGENTS.md, not
  env.md.
- `uv pip` works against any venv or the system; always run it with
  the intended venv on `PATH` (or via `uv pip --python .venv/bin/python`),
  so the install lands in the right place.
