# pi skills specification (condensed)

Condensed from pi's `docs/skills.md` (in the pi install tree, not this
repo).  An agent editing skills needs the limits without hunting for
that file.  The authoritative source is the pi documentation.

## Directory and discovery

- A skill is a directory containing `SKILL.md`; pi also accepts some
  standalone Markdown skills, but the directory form is portable.
- Locations: the user skills directory, project `.agents/skills/`
  (this repo), and `~/.agents/skills/`.  Project `.agents/skills/` is
  discovered from the working directory through its ancestors, stopping
  at the repository root.
- Directories containing `SKILL.md` are discovered recursively.

## Frontmatter fields

| Field | Purpose |
|---|---|
| `name` | Command and display name |
| `description` | Routing description shown to the model |
| `license` | License name or bundled license file |
| `compatibility` | Environment requirements |
| `metadata` | Additional key-value metadata |
| `allowed-tools` | Experimental pre-approved tool list |
| `disable-model-invocation` | Hide from automatic model selection |

- `name` and `description` are the only fields this repo's skills use
  so far; the rest are optional.
- Malformed `SKILL.md` files and declared skills without a description
  are not loaded.  Name collisions keep the first discovered skill and
  produce a warning.

## Limits and name rules

- name: lowercase letters, digits, hyphens; no leading, trailing, or
  consecutive hyphens; at most 64 characters.
- description: at most 1024 characters.
- pi neither requires nor warns when the declared name differs from the
  parent directory, but other implementations may enforce it; keep them
  matching for portability.

## Loading model

- At startup pi scans skills and adds only the name, description, and
  path to the system prompt.  Full instructions load when a task
  matches.
- A model can miss a relevant skill; `/skill:name` forces loading.
  Arguments after `/skill:name` are appended to the instructions as a
  user request.
- `disable-model-invocation: true` makes a skill available only through
  its explicit `/skill:name` command.

## Bundled-file paths

- Refer to `scripts/`, `references/`, and `assets/` by path relative to
  the skill directory.  Pi tells the model where the skill lives so it
  can resolve those paths.
