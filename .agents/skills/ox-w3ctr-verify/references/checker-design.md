# Designing a checker that can disagree with the code

Two false alarms from the 2026-09 OINFO verification round, and what they
generalize to.  Both were produced by checkers that looked reasonable and were
wrong; neither was caught by re-reading the implementation — only by
cross-checking with a second tool.

## Case 1 — scanning markup that is not markup

First HTML check: regexp `id="..."` / `href="#..."` over the exported text.
Verdict: 46 of 57 documents have a duplicate `id="toc"`, all 57 have a dangling
`href="#ref"`.

Wrong.  The output starts with the W3C header blurb, a comment that *lists*
required ids and classes as examples:

```
 *   - #toc for the Table of Contents (<nav id="toc">)
 *   - ul.index for Indices (<a href="#ref">term</a><span>, in §N.M</span>)
```

plus a JSDoc block inside `<script>`.  Strip `<!--…-->`, `<style>…</style>` and
`<script>…</script>` and both "findings" vanish (0 duplicate ids, 0 dangling
links).

## Case 2 — a walker that counted twice

Second attempt: parse with `libxml-parse-html-region` and walk the DOM, which
should be immune to comments.  Verdict: every id appears exactly twice, in
every document — including ids that `grep -c` counts once.

Wrong.  The walker double-visited nodes (walker=2, grep=1 for every id), so the
"duplicate" list was pure artefact.  A DOM-based check needs the DOM's node
shape handled correctly; when a checker reports something as systematic as
"everything is duplicated", suspect the checker before the code.

## What to do instead

- **Strip non-markup before scanning** (`<!--…-->`, `<style>`, `<script>`), and
  say so in the script header.  Anything that quotes markup as *text* will
  otherwise be counted.
- **Cross-check a surprising verdict** with a second, independent tool before
  believing it: `grep -o … | sort | uniq -d` against a DOM walk, a raw diff
  against a hash comparison.
- **Prefer differential checks**: comparing the layer-on and layer-off builds
  needs no model of what the output *should* be, so it cannot encode the same
  mistake as the implementation.  (Same for "the same build run twice must
  agree" — that is what exposed the random reference ids.)
- **Explain every difference**, don't normalize it away.  Normalization is for
  things proven environmental (timestamp, random ids); each such rule should
  come with the reason and a way to check it (here: re-run the same build and
  watch the ids change).
- **Beware checkers whose verdict depends on the environment**: a test that
  reads `ox-w3ctr.el` next to the loaded file skips when only a `.elc` is
  installed — the reported baseline has to mention that.
- **Narrow the claim to the corpus**: "no unresolved links in these 57
  documents" is evidence; "links resolve" is not.
- **A shared placeholder can lie about uniqueness.** Mapping every random
  `orgXXXXXXX` id to one value (what `verify-html.el` does without `RAW=1`)
  makes two anonymous elements collide, and the checker then reports a
  duplicate id that is pure artefact — the corpus showed two such documents
  while the raw ids had none.  `verify-corpus.el` therefore counts duplicate
  ids on the raw ids and normalizes only for its `norm=` hash; if one string
  must serve both jobs, give each distinct value its own placeholder.

## Case 3 — indenting a file that was never loaded

`indent-region' needs the file's own macro definitions: the test helper
macros carry `(declare (indent ...))' specs, and without them Emacs falls
back to generic function indentation.  Running `indent-region' over
`ox-w3ctr-tests.el' without loading it first reported 523 non-canonical
lines; loading it first, 32.  Load before indenting (the recipe in
`harness.md' does).

## Case 4 — "compile to a temp file" that wrote the repo instead

`byte-compile-file''s second argument is `LOAD', not an output path.
`(byte-compile-file "ox-w3ctr.el" "/tmp/out.elc")' compiles to the
`ox-w3ctr.elc' beside the source and then tries to *load* `/tmp/out.elc'
(which does not exist).  A wrapper meant to keep the build out of the tree
silently refreshed the stale `ox-w3ctr.elc' instead -- and because `load'
prefers the newer of `.el'/`.elc', that changed what later runs read.
