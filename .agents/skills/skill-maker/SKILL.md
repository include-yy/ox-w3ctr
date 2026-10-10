---
name: skill-maker
description: Create and review pi skills — write SKILL.md with a routing-grade description, lay out scripts/references/assets, and audit existing skills against the Agent Skills spec. Use when asked to make a new skill, or to review or fix a skill's description or structure.
---

# Making and reviewing skills

A skill is a directory with `SKILL.md` (frontmatter + instructions) plus
optional `scripts/`, `references/`, and `assets/`.  Pi advertises only
the name, description, and path at startup; the full instructions load
only when the task matches.  The description is therefore routing
information, not documentation — it must answer "when should the agent
load this?" precisely.

Hard limits and the field list are in `references/pi-skills-spec.md`;
the description's shape is in `references/description-guide.md`.

## Create a skill

1. **Pin the job.**  Before writing any file, answer in one line each:
   - what it does,
   - when it applies (the trigger conditions, as a scannable list),
   - when it does *not* apply (negative routing, to avoid false loads).

2. **Write the description** from those three answers, following
   `references/description-guide.md`.  Most skills need under 300
   characters; 1024 is the hard cap.

3. **Lay out the directory** under `.agents/skills/<name>/`:
   - `SKILL.md` — frontmatter, then a title, a numbered Workflow of
     runnable steps, and Rules.  Refer to bundled files by path
     relative to the skill directory.
   - `scripts/` — executable helpers the instructions call.  State
     their runtime dependencies (Emacs, Python, Node, shell) in the
     SKILL.md text.
   - `references/` — long or seldom-needed detail, pulled in on demand.
   - `assets/` — templates or data the scripts consume.

4. **Verify** against `references/pi-skills-spec.md`:
   - name: lowercase letters, digits, hyphens; no leading, trailing,
     or consecutive hyphens; at most 64 chars; matches the directory.
   - description: present, at most 1024 chars, states what + when.
   - scripts resolve by relative path and run from the repo root.

## Review a skill

- Does the description answer what + when (and ideally when-not)?  A
  "Helps with X" description fails routing and must be rewritten.
- Is the frontmatter valid per `references/pi-skills-spec.md`?
- Does the Workflow consist of commands the agent can actually run,
  with paths relative to the skill directory rather than the repo root?
- Are the scripts referenced, and are their dependencies declared?
- Does the text drift from what the scripts do?  Run each script once
  to confirm the instructions match its real behaviour.

## Rules

- The description is the routing contract; SKILL.md is the procedure.
  Do not move trigger conditions out of the description into the body,
  or the skill will be skipped when it should load.
- `scripts/` hold mechanics; keep policy in the SKILL.md text.  A
  script must not encode a decision the model should make.
- Keep the directory name and the `name` field identical; pi tolerates
  a mismatch but other implementations do not.
- This skill itself follows the same rules: review it with its own
  checklist when you change it.
