---
name: msys2-git
description: Configure MSYS2's git to reuse the Windows credentials and SSH keys, so GitHub HTTPS and SourceHut SSH remotes work under the MSYS2 shell. Use when git push/fetch under MSYS2 fails with authentication errors.
---

# Bridging git across the two homes

Under the MSYS2 shell, git uses `/home/<user>`, but the identity, the
GitHub credentials and the SSH keys all live in the Windows profile
(`C:\Users\<user>`).  Three gaps to bridge:

## 1. Identity

Copy `name` and `email` from the Windows `~/.gitconfig`
(`$(cygpath -u "$USERPROFILE")/.gitconfig`), then:

```bash
git config --global user.name "<name>"
git config --global user.email "<email>"
```

## 2. GitHub HTTPS

MSYS2's git has no credential helper.  Store the GitHub token from
Windows' Git Credential Manager:

```bash
bash .agents/skills/msys2-git/scripts/store-github-token.sh
```

The script reads the token via Git Bash's GCM, writes it to
`~/.git-credentials`, and sets `credential.helper = store`.  The token
is kept in plaintext (a GitHub PAT), with `chmod 600`.

## 3. SourceHut SSH

Symlink the Windows `.ssh` into the MSYS2 home, only if absent:

```bash
[ -e ~/.ssh ] || ln -s "$(cygpath -u "$USERPROFILE")/.ssh" ~/.ssh
```

## Verify

```bash
git fetch gh master      # GitHub HTTPS
git fetch origin master  # SourceHut SSH
```

## Rules

- `credential.helper = store` keeps the token in plaintext
  `~/.git-credentials`; prefer it over a broken push, but know the
  trade-off.  A native GCM package would be better, but MSYS2 has none
  in its repos.
- Do not set `db_home: windows`: it pollutes the whole MSYS2
  environment.  Bridge git only, as this skill does.
- Never print the token; the script already avoids it.
- These are per-machine, local fixes; keep them out of committed
  files.  AGENTS.md records the *convention*, env.md the machine facts.
