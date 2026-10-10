---
name: env-detect
description: Detect this checkout's machine-specific environment (Emacs, Python, Org sources, proxy, remotes, Node) and write it to .agents/env.md. Use at session start when .agents/env.md is absent or stale, or when a value fails an invariant in AGENTS.md.
---

# Detecting the environment

Run the probe script and turn its output into `.agents/env.md`.  The
script is the only source of the values: do not hand-probe.  If the
script fails a fact, fix the environment or ask the user — never guess.

## Workflow

1. **Run the probe.**  From the repository root:

   ```bash
   bash .agents/skills/env-detect/scripts/detect-env.sh
   ```

2. **Check for `FAIL` lines.**  Each `FAIL key: reason` means a fact
   could not be verified.  The script already tried the env.md record
   and the Emacs bundle, so a FAIL means those are gone too: ask the
   user for the location, or if they do not know, scan for it
   (`find / -name 'emacs*.exe' 2>/dev/null`,
   `find / -name ox.el 2>/dev/null`).  Never write a guessed value; a
   fact that stays unresolved is simply absent.

3. **Write `.agents/env.md`** from the `key=value` lines, using the
   template below.  `Detected:` is `date +%Y-%m-%dT%H:%M:%S%z`.

## When the user names a specific source

If the user asks for a particular Org checkout or Emacs build, do not
edit `.agents/env.md` by hand.  Re-run the probe with the override and
write the script's output:

```bash
ORG_SOURCES=/path/to/org-mode/lisp EMACS=/path/to/emacs.exe \
  bash .agents/skills/env-detect/scripts/detect-env.sh
```

The script validates each path (Org must contain `ox.el`; Emacs must
be version >= 31) before it prints the `key=value` line; a bad path
produces a `FAIL` line instead.  Write only what the script prints.

## When a recorded value is gone

An `org_previous=` line means the Org path recorded in `.agents/env.md`
was gone and the probe fell back to the Emacs bundle.  Report this to
the user before writing env.md — do not silently overwrite their
choice with the bundle path.

## Template

```markdown
# Agent environment (auto-detected)

Detected: <timestamp>

Machine-specific facts for this checkout.  Re-detect (and rewrite this
file) when it is absent, when a probe below disagrees, or when a value
fails the invariant recorded in AGENTS.md.

| Fact        | Value            | Verification                                    |
|-------------|------------------|-------------------------------------------------|
| Emacs       | `<emacs>`        | `--version` → "<emacs_version>"                 |
| Python      | `<python>`       | `--version` → "<python_version>"<python3 note>  |
| Org sources | `<org>`          | `ox.el` present                                 |
| Shell       | `<shell_flavor>` | `cygpath -w $BASH` → "<bash>"                   |
| Proxy       | `<proxy>`        | `curl https://github.com` → 200                 |
| Node        | `node` / `npm`   | `--version` → <node> / <npm>                    |

## Remotes (this checkout)

- `<remote>` = `<url>` — HTTPS or SSH
...
```

Fill the `<...>` placeholders from the script output.  The `<python3
note>` is `; not python3 (Windows Store stub)` when the script prints
`python3_broken=store-stub`, and empty otherwise.  In the Remotes
section, one entry per `remote_<name>=<url>` line, noting which need
the proxy above (HTTPS) and which do not (SSH).

## Rules

- The script, not the agent, decides the values.  Copy the output
  verbatim into the template; do not re-verify by hand unless a `FAIL`
  line asks for it.
- A fact with no `FAIL` and no `key=value` line is simply absent; do
  not invent it.
- `.agents/env.md` is gitignored: never stage or commit it.
