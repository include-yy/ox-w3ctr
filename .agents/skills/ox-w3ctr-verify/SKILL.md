---
name: ox-w3ctr-verify
description: Verify changes to ox-w3ctr beyond the ERT suite — build the shipped (`org-w3ctr-oinfo-enabled` on) flavour, run this skill's corpus scripts, compare two revisions of the source, and design checkers that can disagree with the code. Build the nil flavour as well only when the change is to the cache itself. Use when a change touches shared machinery (options/OINFO, references and anchors, head/template/preamble, filters), when a definition-time switch is added or flipped, when definitions or option lists are reordered, or when asked to check exports end to end.
---

# Verifying ox-w3ctr changes

## When this applies

- Shared machinery changed: `:options-alist`, OINFO (`t--pget`/`t--pput`,
  `t-oinfo-enabled`), references/anchors (`t--reference`), head/template/
  preamble, filters.
- A new definition-time switch or feature flag exists.
- The request is "check the exports", "does it still work", "全局检查一个".

Unit tests cannot see any of this; they assert single elements.

## The four layers

| Layer | Catches | Where | In git |
|---|---|---|---|
| ERT unit tests | element output, contracts | `ox-w3ctr-tests.el` | yes |
| Source-scanning tests | drift between lists and code (a cached key never read, a test namespace colliding with production) | same file | yes |
| Corpus harness | global invariants: cache transparency, HTML validity, anchor integrity, retention, timings | this skill's `scripts/` (builds and outputs under `tools/build`, gitignored) | no |
| Load-time assertions | platform preconditions (`plist-put` writes in place) | head of `ox-w3ctr.el` | yes |

## Workflow

1. **ERT first** — the command and the current baseline are in AGENTS.md
   ("Running the tests"), which also records the dormant nil-build numbers and
   the source-less (`.elc`-only) caveat:
   `emacs --batch -L . --eval "(setq load-prefer-newer t system-time-locale (symbol-name 'C))" -l ox-w3ctr-tests.el -f ert-run-tests-batch-and-exit`
2. **Build the shipped flavour and run the corpus harness** — both flavours
   only when the change is to OINFO itself.  The recipe is in
   `references/harness.md`: `verify-oinfo.el` is the OINFO instrumentation
   (target document, `PIDS`/`STATS`/`ABORT`), `verify-corpus.el` is the bulk
   pass (per-document `hash=`/`norm=`/`refs=`, HTML and anchor checks in the
   same export).  Compare the `norm=` columns; both scripts export the corpus
   once each, so run them once, at the end.
3. **Differential check**: for an optional layer, the layer-on and layer-off
   builds must produce byte-identical output.  Normalize only what is
   environmental (the export-timestamp comment, Org's random reference ids),
   and *prove* a difference is environmental by re-running the same build.
4. **Report numbers, not adjectives** — hashes, counts, seconds, and the
   command that produced them.

## Source-level checks (refactors, reorders)

`scripts/verify-forms.el OLD.el NEW.el` proves that a move/reorder of
`defgroup`/`defcustom`/`defconst`/`defvar`/`defsubst`/`defun` forms changed
nothing but order (it compares each form's `prin1-to-string`);
`scripts/verify-options.el` does the same for `:options-alist`.  Materialize
the old revision first:

```bash
mkdir -p /tmp/v && git show HEAD:ox-w3ctr.el > /tmp/v/old.el
emacs --batch -Q -l .agents/skills/ox-w3ctr-verify/scripts/verify-forms.el \
      /tmp/v/old.el ox-w3ctr.el        # RESULT: identical definitions, order-only change
```

## Rules that keep a checker honest

- A checker is only worth running if it *can* disagree with the code.  Prefer
  comparing two builds, parsing with a third-party parser (`libxml`), counting
  anchors, measuring wall time — never restate the implementation's own
  assumption (that fails exactly when the code does).
- Cross-check any surprising verdict with a second, independent tool before
  believing it (grep vs DOM, signature of a difference vs a raw diff).  See
  `references/checker-design.md` for two false alarms this cost, and what they
  teach.
- If a test's verdict depends on something outside the repo (a source file, a
  document corpus), say so next to the baseline number.
- Timing: `bench-oinfo.el` compares the cache against `plist-get` inside one
  build; reverse the order for a second round.  A win that disappears when
  the order changes is noise.
- **Run the recipe; do not improvise around it.**  A wrapper written outside
  the skill drifts from the recipe, and the next run has to rediscover what it
  did.  If the recipe is missing a step, add a script to `scripts/` and update
  `references/harness.md`.
