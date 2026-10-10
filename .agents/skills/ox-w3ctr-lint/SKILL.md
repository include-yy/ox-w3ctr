---
name: ox-w3ctr-lint
description: Run the cheap static checks on ox-w3ctr.el — strict byte-compile, checkdoc, the (pure t) call-graph scan, ftype gaps, the non-ASCII scan, and indentation. Use on any change, before the heavier corpus verification.
---

# Static checks on ox-w3ctr

Cheap checks from the repo root; each catches a class the ERT suite
cannot.  Run them on any change, before the heavier corpus verification
(the `ox-w3ctr-verify` skill).

| Script | Checks |
|---|---|
| `strict-compile.el` | byte-compile with `byte-compile-error-on-warn` and `byte-compile-warnings` at `t`; any warning fails |
| `checkdoc.el` | checkdoc over `ox-w3ctr.el` |
| `pure-scan.el` | a call-graph scan for `(pure t)` functions reaching `t--pget`/`t--pput` (AGENTS.md forbids it) |
| `scan-ftype.el` | counts the functions lacking `(declare (ftype ...))` |
| `scan-nonascii.el` | flags non-ASCII characters in `ox-w3ctr.el` outside the one whitelisted glyph (the back-to-top ↑, rendered HTML content) |
| `indent-check.el FILE` | lines `indent-region` would change; **LOADs FILE first** (see `../ox-w3ctr-verify/references/checker-design.md`) |

## Workflow

```bash
emacs --batch -L . -l scripts/strict-compile.el
emacs --batch -L . -l scripts/indent-check.el ox-w3ctr.el
emacs --batch -L . -l scripts/indent-check.el ox-w3ctr-tests.el
emacs --batch -Q  -l scripts/pure-scan.el        # no VIOLATION line
emacs --batch -Q  -l scripts/scan-ftype.el
emacs --batch -Q  -l scripts/scan-nonascii.el    # NON-ASCII-OK
```

Paths are relative to this skill directory (`.agents/skills/ox-w3ctr-lint/`).
