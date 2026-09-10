# Session forking and git branches

`/fork` clones the current persisted conversation into a new peer session,
optionally pairing it with an isolated git worktree or a git branch. `/checkout`
switches both the git branch and the chat linked to it.

## `/fork`

```
/fork [--worktree|--no-worktree] [--branch <name>|-b <name>] [DIRECTIVE]
```

`DIRECTIVE` is an optional first prompt for the forked session. `/fork`
requires a persisted interactive session with at least one substantive turn.

With no flags, `/fork` asks how to isolate the fork:

- **Use a new worktree** — create a managed worktree under
  `~/.haskell-agent/worktrees/<repo>/YYYY-MM-DD-<hex8>` and fork there.
- **Share current workspace** — fork in place in the current checkout.

The flags choose directly instead of prompting:

- `--worktree` — same as "Use a new worktree".
- `--no-worktree` — same as "Share current workspace".
- `--branch <name>` (alias `-b`) — create `name` in the current checkout
  (`git checkout -b <name>`) and link the in-place fork to it. The current
  directory must be a git repository and the branch must not already exist;
  outside a repository the name degrades to the fork title.

`--worktree` and `--branch` are mutually exclusive. `--at <turn>` (fork from a
historical point) is not supported in this version; use `/rewind` first and
then `/fork`.

### What a fork copies

A fork is a brand-new persisted session:

- a fresh session id and timestamp, a derived title (or the requested branch
  name outside a repository, or a `--branch`-derived title), and the target
  working directory;
- the **active transcript only** — everything after the last
  `TranscriptReset` is dropped;
- allowlisted durable artifacts (`plan.md`, `agent.log`); symlinked artifacts
  are rejected and the fork is rolled back.

### Git branch linking

The session store records a `git_branch` column (migration v105) so a session
remembers the branch it was forked onto:

- `--branch <name>` records `name`.
- `--worktree` records the branch only when one was actually created. A branch
  is created when a fetch base is resolved (the default
  `worktree.fetchLatestUpstream = true` and the repository has a remote);
  the branch is named after the worktree directory. With no remote (or with
  `fetchLatestUpstream = false`) the worktree is a detached HEAD and no branch
  is recorded.
- `--no-worktree` and "Share current workspace" do not record a branch.

## `/checkout`

```
/checkout <branch>
```

From inside a git repository, `/checkout`:

1. runs `git checkout <branch>` — if that fails (for example the branch is
   already checked out in another worktree), the command aborts and surfaces
   git's error;
2. resumes the newest session whose `git_branch` equals `<branch>` and whose
   working directory is inside the current project root;
3. if no such session exists, starts a fresh session on the now-checked-out
   branch.

This round-trips the `/fork --branch` flow. Worktree forks (from
`/fork --worktree`) record their branch, but because git forbids checking out a
branch that another worktree already holds, `/checkout` cannot switch into one
from a different checkout — resume those sessions with `/resume` or by opening
the worktree directly.

## Related commands

- `/worktree` — start a fresh session in a new git worktree (no fork).
- `/resume [ID]` — resume a persisted session by id.
- `/rewind` — move the conversation back to a previous turn before forking.
