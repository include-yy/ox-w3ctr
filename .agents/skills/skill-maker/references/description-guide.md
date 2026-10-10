# Writing a routing-grade description

The description is the only text the model sees before deciding whether
to load a skill.  It is routing information, not documentation.  Model
it on a command's `--help`: scannable, with the trigger conditions
standing out instead of buried in prose.

## Shape

Three short parts, in order:

1. **What** — one clause naming the job.
2. **When** — the trigger conditions, as a list or a compact
   enumeration, each starting with a concrete verb or noun.
3. **When not** (optional but valuable) — the common case that looks
   similar but must *not* load this skill.

## Rules

- "State both what the skill does and when it applies" (pi docs).  A
  description like "Helps with PDFs" has no routing value.
- Prefer concrete triggers over abstractions: "when a defcustom moves
  or an option list is reordered" beats "when configuration changes".
- Keep it under about 300 characters when possible; 1024 is the hard
  cap.
- Do not duplicate the Workflow.  The description routes; SKILL.md
  instructs.  Only the trigger conditions belong in the description.
- Name the skill's own scripts or references only when they are part of
  the trigger ("run this skill's corpus scripts" is a fine what).

## Example (good)

    Verify ox-w3ctr beyond ERT: build the shipped flavour, run corpus
    checks, diff two revisions.  Run when a change touches options/OINFO,
    references/anchors, head/template/preamble, or filters; when a
    definition-time switch is added or flipped; when option lists are
    reordered.  Build the nil flavour only for cache changes; skip for
    routine single-function edits.

## Example (bad)

    Helps with verification.

    (no what/when split, no triggers, no negative routing)
