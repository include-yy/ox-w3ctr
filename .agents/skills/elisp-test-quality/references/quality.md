# The judgment pass

The scripts in `scripts/` catch mechanical candidates.  These are the
qualities they cannot see; read the tests for them.

## The one question

**If I break the code's assumption, does this test go red — for the
right reason?**  A test that cannot fail is decoration.  When a checker
is available, prove it: temporarily change the implementation (or the
expected value) and watch the test fail.

## Test the fixture

A helper that many tests share must itself be tested, like any other
contract: if it is broken, every test that uses it passes or fails for
the wrong reason, and the cause is far from the symptom.  Install the
fixture on a real subject, check what it captures, and prove it cleans
up after both a normal and a signalling exit.  The rest of the suite is
evidence for the code, not for the fixture.

## Contract, not implementation

Assert the behaviour the function promises, not the expression it
happens to compute.  A test that mirrors the body (`should` comparing
`(f x)` with the same expression re-derived) passes even when the
promise is wrong, because it fails exactly when the code fails.

For a formatter the exact output *is* the contract, so exact strings are
right — but say so in a comment when the exactness is deliberate (e.g.
"this is ox-html's spacing, kept for compatibility").

## Determinism

No verdict may depend on the clock, the locale, randomness, the network,
the filesystem or a subprocess.  Where the code cannot avoid them, the
test pins the control (`system-time-locale` to C, `text-quoting-style`
to grave) or normalizes the volatile part (timestamps, generated ids).
`ENV-SENSITIVE` from the scanner is the prompt to check which.

## Isolation and leaks

Every test must pass alone and in any order, and must restore what it
changes: bind globals with `let`/`dlet` instead of `setq`; undo
`advice-add`/`fset` in an `unwind-protect`; kill buffers and processes
it creates.  `test-leaks.el` shows what survives the run;
`test-isolation.el` shows order dependence.

## Fixtures the code writes to

A fixture the code mutates must be the object the code keeps.  A helper
that returns a *new* object hides the write: an empty plist passed as
INFO is silently replaced by `plist-put`'s return value, which the
caller discards, so a cache the code stores never persists and the test
passes against a no-op.  Give such a fixture a non-empty plist (e.g.
`(list :the-key nil)`) and assert the write landed.

## Round-trips and counts prove less than they look

A read-after-write, or a lookup counter, often holds under a plain
implementation too: writing a value then reading it back does not show
the value bypassed the plist; a counter bumped on both hit and miss
cannot tell them apart; a cached `nil` and a missed `nil` read the same.
To pin a cache — or any optimization — assert the *divergence* it
creates: after the write, mutate the underlying source of truth and
check the stale value still comes back.  This is mutation thinking done
by hand.

## Boundaries and error paths

Empty, `nil`, blank, zero, negative, malformed input, and every
`signal`.  An error branch without a test (`$e!`/`should-error`) is an
untested contract.

## Readability

Prefer clear over DRY: a table-driven helper (`t-check-element-values`)
removes boilerplate, but deduplicating the expectations hides them.
Name the test after the behaviour; assert one behaviour per test so a
failure points at one cause.  A test whose name no longer matches a
function is a candidate for deletion, not repair.

## Right layer

Pure helpers get unit tests; transcoders get a whole-export test;
cross-file invariants get source-scanning or property tests.  Do not
force one layer to do another's job.
