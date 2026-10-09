# Corpus harness (shipped build; the nil flavour only for OINFO changes)

The scripts live in `scripts/` next to this file; the build and its
outputs go to `tools/build/` (gitignored, like all of `tools/`).  They check
whole exports against a corpus of real documents.

| Script | Checks |
|---|---|
| `verify-oinfo.el` | cache transparency (ON vs OFF hashes), per-key lookup counts, keys never touched, retention (a full export leaves no INFO in the caches, an aborted one does, the opt-in `org-w3ctr-oinfo-cleanup-before-export` hook clears it), 20 single-run timings |
| `verify-html.el` | `libxml-parse-html-region` parses each export, no `id` twice, every internal `#...` link resolves (`RAW=1` keeps Org's random ids) |
| `bench-oinfo.el` | `t--pget` vs `plist-get` on a real INFO plist captured from an export: it times `(funcall 'closure info)`, which is what the inlined call site compiles into, so no compilation step is needed |
| `dump-export.el` | export one document to a file, for diffing two builds |

## Recipe (MSYS2 bash; `$EMACS` = your emacs)

```bash
cd <repo>
SKILLDIR="$PWD/.agents/skills/ox-w3ctr-verify/scripts"
# NB: this wipes tools/build, including the previous run's outputs; copy
# anything you want to compare against (e.g. `cp tools/build/*.txt /tmp/`).
rm -rf tools/build && mkdir -p tools/build/on
cp ox-w3ctr.el tools/build/on/; cp -r assets tools/build/on/
mkdir -p tools/build/on/jstools; cp jstools/index.js jstools/package*.json tools/build/on/jstools/
(cd tools/build/on && "$EMACS" --batch -L . --eval '(byte-compile-file "ox-w3ctr.el")')
# Byte-compile output must be warning-free — treat a warning as a finding
# (2026-09: a stray-declare warning exposed a truncated docstring that had
# silently disabled a function's ftype check).  Keep it on the terminal;
# do not redirect it away.

DRAFTS=/path/to/drafts/ TARGET=2026-01-01-gv-history/index.org
(cd tools/build/on && DRAFTS="$DRAFTS" TARGET="$TARGET" OINFO=on \
   "$EMACS" --batch -L . -L "$SKILLDIR" -l verify-oinfo > ../oinfo-on.txt 2>/dev/null)
(cd tools/build/on && DRAFTS="$DRAFTS" OINFO=on \
   "$EMACS" --batch -L . -L "$SKILLDIR" -l verify-corpus > ../corpus-on.txt 2>/dev/null)
grep '^RESULT' tools/build/corpus-on.txt   # 0 errors, 0 parse errors, 0 dup ids, 0 unresolved

# Self-check on the checker: the same build twice must agree on norm=.
(cd tools/build/on && DRAFTS="$DRAFTS" OINFO=on \
   "$EMACS" --batch -L . -L "$SKILLDIR" -l verify-corpus > /tmp/corpus-on-2.txt 2>/dev/null)
diff <(grep '^BATCH ' tools/build/corpus-on.txt | awk '{print $2, $4}' | sort) \
     <(grep '^BATCH ' /tmp/corpus-on-2.txt     | awk '{print $2, $4}' | sort)

# `hash=' is the raw export and moves between runs for a document that holds
# anonymous elements — the `refs=' column says which ones (4 of the 57 today).
# Never diff raw hashes; diff `norm=' (timestamp and those ids normalized).

# the micro benchmark (best of three runs per key; it compares the cache
# against plist-get inside one build, so the shipped build is enough)
(cd tools/build/on && DRAFTS="$DRAFTS" OINFO=on \
   "$EMACS" --batch -L . -L "$SKILLDIR" -l bench-oinfo)   # MICRO-REAL lines

# one document to a file, for diffing the two builds
(cd tools/build/on && ORG="$DRAFTS$TARGET" OUT="$PWD/dump.html" \
   "$EMACS" --batch -L . -L "$SKILLDIR" -l dump-export)
```

Run the recipe as written.  If something is missing, add it to this skill's
`scripts/` — a wrapper written elsewhere drifts from the recipe, and the
next run then has to rediscover what it did (2026-09: five such wrappers,
three extra corpus passes, and a raw-hash baseline that could not work).

Each script exports the corpus once per run, so the two above cost two passes,
~10 s each.  Keep your edits together and run this once at the end rather than
after every edit.

```bash
# Cache transparency — only when the change is to OINFO itself.  Build the
# nil flavour and diff the norm= columns; that is the whole point of it.
mkdir -p tools/build/off
for p in assets jstools; do cp -r "tools/build/on/$p" tools/build/off/; done
cp ox-w3ctr.el tools/build/off/
(cd tools/build/off && "$EMACS" --batch -L . \
   --eval '(setq org-w3ctr-oinfo-enabled nil)' --eval '(byte-compile-file "ox-w3ctr.el")')
(cd tools/build/off && DRAFTS="$DRAFTS" OINFO=off \
   "$EMACS" --batch -L . -L "$SKILLDIR" -l verify-corpus > ../corpus-off.txt 2>/dev/null)
diff <(grep '^BATCH ' tools/build/corpus-on.txt  | awk '{print $2, $4}' | sort) \
     <(grep '^BATCH ' tools/build/corpus-off.txt | awk '{print $2, $4}' | sort)   # must be empty
```

`-L .` is the flavour build, `-L "$SKILLDIR"` the scripts (an MSYS-style path
works — MSYS2 converts it for the native Emacs).  Documents are only read.

## Normalize before comparing

Two things in the output are not part of its meaning:

- the export-timestamp comment (`<!-- 2026-…Z -->`) — a fresh time per export;
- **Org's random reference ids** (`orgXXXXXXX`, 7 or 8 random hex digits).
  Org's `org-export-new-reference` (ox.el) hands one to every element that
  reaches `(t (org-export-get-reference datum info))` in `t--reference`, i.e.
  elements without `CUSTOM_ID`/`#+NAME`/`ID` — an unnamed `#+begin_example`,
  for instance.  This makes such exports **unreproducible** (same flavour, two
  runs, different ids); the fix is an explicit id in the document, and the
  back-end-side item is the "explicit anchors instead of random reference ids"
  note in AGENTS.md.  `verify-corpus.el` normalizes them into `norm=` and
  counts them in `refs=` (4 of the 57 documents today, and the affected set
  varies with the run) — never diff the raw `hash=`.
- a **stale saved baseline**.  `cp tools/build/*.txt /tmp/` keeps the last
  run, but if that run predates a change to shared output (v0.2.12 rewrote
  headlines, so every document's `norm=` moved), the diff flags all 57
  documents.  Compare against the parent revision instead:
  `git show HEAD~1:ox-w3ctr.el`, build it, run `verify-corpus` once, and
  diff the two `norm=` columns.

So: normalize both, then compare.  When a document still differs, dump both
builds (`dump-export.el`) and diff the *raw* HTML — a difference must be
explained (a real one, or another environmental source), not normalized away
by reflex.

## What the corpus has established (2026-09, 57 documents, 4.0 MB of HTML)

- Cache transparency: identical output in both flavours for the whole corpus
  (`norm=` equal on all 57), 0 parse errors, no duplicate ids (checked on the
  raw ids), every internal link resolves.  `verify-corpus.el` reports all of
  that in one pass; `verify-html.el` remains for a single document.  Since
  2026-09 the nil build is not built routinely: re-run the differential only
  when a change touches the cache.
- Retention: a full export leaves 0 oclosures holding INFO; an aborted export
  leaves the caches populated and the next export still sees them unless the
  opt-in hook is installed.
- Coverage gap to keep in mind: the corpus contains no timestamps, so the
  cache-only-write path (`:html-timezone`, `:html-export-timezone`, where
  `t--pput` writes the cache and not the plist) has **no** end-to-end coverage
  — only ERT.  A document with a timestamp closes it.
- Coverage gap: no corpus document sets =HTML_LINK_UP= / =HTML_LINK_HOME=
  / =HTML_LINK_NAVBAR= / =HTML_HOME/UP_FORMAT= (grep 2026-10-03: 0
  keyword uses; one document quotes them in prose), so the navbar and
  legacy home/up path has **no**
  corpus coverage — only ERT.  The Legacy home and up pass (2026-10)
  ran its differential anyway: 57/57 =norm== identical (HEAD vs
  worktree), 0 errors, len=3979676 both — empty *because* of this gap,
  not evidence for the path.
- Performance, measured on a 174-pair INFO from a real export: the cache wins
  only when the key sits deep enough that `plist-get` walks far — ratio
  ≈ 0.53–0.65 for keys at pair #66 and #73, but ≈ 0.9–1.4 (a wash or slower)
  for an early key at #21; per-key ratios swing by ±0.4 between rounds, so
  read them as orders of magnitude, not fractions.  End-to-end over 57
  documents the difference is below the noise (8.4 s both ways in the second
  round).  Hence: cache the keys that are both deep and frequently read, and
  do not expect a speedup from caching in general.

## Static checks (run on any change)

Cheap, from the repo root; each catches a class ERT cannot.

| Script | Checks |
|---|---|
| `strict-compile.el` | byte-compile with `byte-compile-error-on-warn` and `byte-compile-warnings` at `t`; any warning fails |
| `checkdoc.el` | checkdoc over `ox-w3ctr.el` |
| `pure-scan.el` | a call-graph scan for `(pure t)` functions reaching `t--pget`/`t--pput` (AGENTS.md forbids it) |
| `scan-ftype.el` | counts the functions lacking `(declare (ftype ...))` |
| `scan-nonascii.el` | flags non-ASCII characters in `ox-w3ctr.el` outside the one whitelisted glyph (the back-to-top ↑, rendered HTML content) |
| `indent-check.el FILE` | lines `indent-region` would change; **LOADs FILE first** (see `checker-design.md`) |

```bash
emacs --batch -L . -l scripts/strict-compile.el
emacs --batch -L . -l scripts/indent-check.el ox-w3ctr.el        # 2: the license-alist alignment
emacs --batch -L . -l scripts/indent-check.el ox-w3ctr-tests.el  # 0
emacs --batch -Q  -l scripts/pure-scan.el                        # no VIOLATION line
emacs --batch -Q  -l scripts/scan-ftype.el                       # 3 to fix (+ the end-user t-publish)
emacs --batch -Q  -l scripts/scan-nonascii.el                 # NON-ASCII-OK
```

## RPC transport harness

Only when the JSON-RPC transport changes.  `server.py` is a
Content-Length stdio JSON-RPC server (`jsonrpcserver`) standing in for the
node helper; `node_client.py` and `e2e.el` drive the real thing.

| Script | Checks |
|---|---|
| `server.py` | stdio JSON-RPC; methods ping/add/echo/tex2mml/tex2svg/nope.  NB `jsonrpcserver` 5.x methods must return `Success(...)`/`Error(...)` |
| `client.py` | raw framing: success, `-32000`, `-32601`, `-32600` for a string param, a notification gets no reply |
| `client.el` | the same over a real `jsonrpc-process-connection`, including restart |
| `stderr_server.py` | a server that writes to stderr |
| `stderr.el`, `stderr-our.el` | the `*NAME stderr*` coupling; the latter through `org-w3ctr--jrpc-connect` |
| `node_client.py` | the real node helper (Content-Length + object params) |
| `e2e.el` | a real `org-export-string-as` through MathJax, expecting `<math>` |

```bash
python scripts/client.py
emacs --batch -L . -l scripts/client.el
emacs --batch -L . -l scripts/stderr-our.el
python scripts/node_client.py
emacs --batch -L . -l scripts/e2e.el
```

The Elisp clients find `server.py` next to themselves; `node_client.py`
and `e2e.el` need the node helper, so run them from the repo root.
