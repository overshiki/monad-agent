# haskell-agent

An independent agent harness, written in Haskell.

<img width="1426" height="871" alt="Screenshot 2026-08-23 at 10 43 49 PM" src="https://github.com/user-attachments/assets/9da99007-484a-4c8a-9bb1-ca35abf8ae05" />

## Try it out

Build from source (requires [GHC](https://www.haskell.org/ghcup/) and `cabal`):

```bash
git clone https://github.com/overshiki/haskell-agent.git
cd haskell-agent
scripts/setup-cabal-build.sh
cabal install agent-cli:exe:monad-cli
monad-cli
```

## Supported LLM Providers

- OpenAI (Subscription)
- xAI (Subscription)
- Claude (Subscription)
- OpenRouter (API billing)
- DeepSeek (API billing)
- Kimi (Moonshot, API billing)
- Google Gemini (Google account or AI Studio API billing)

## What is distinctive

Most agent harnesses are effectively untyped imperative programming
environments. A model emits loosely structured commands that mutate files,
processes, conversation state, and other shared resources. Correctness depends
on conventions enforced at runtime, often after effects have already begun.

`haskell-agent` is an exploration in a different direction. Model output is
treated as untrusted input at the boundary. Accepted actions are decoded into
typed values, state changes are expressed as pure transformations where
possible, and effects are interpreted explicitly by the runtime. The model
remains probabilistic; the environment in which its actions execute does not
have to be.

- **A functional agent runtime:** protocol states, tool policies, transport
  ownership, UI transitions, and agent lifecycles are modeled with algebraic
  data types. Pure transformations are separated from effectful boundaries,
  while STM coordinates shared concurrent state.
- **GHCi as part of the agent architecture:** every model gets a persistent
  typed workspace. The harness distinguishes pure expressions from effectful
  actions, preserves bindings across calls, and recovers or restarts GHCi when
  interruption makes its state uncertain.
- **First-class model dialects:** providers own authentication, billing, and
  transport, while dialects own the model-facing prompt, tool surface, schema
  conventions, project-instruction formatting, and subagent protocol. This
  keeps Codex-style and Grok Build behavior intact even when a transport such
  as OpenRouter serves models from several families.
- **Cross-provider state and billing policy:** provider transitions preserve
  the pending turn and durable session state. Credential failover understands
  account cooldowns and prevents automatic fallback from silently converting
  subscription usage into API-credit spending.
- **Explicit response ownership:** reusable WebSocket requests carry
  generation-scoped ownership. If an exchange is interrupted, malformed, or
  returned before its terminal frame, the connection is poisoned rather than
  risking old frames entering a later response.
- **Types as a path toward safer agency:** typed tool decoding, approval rules,
  and execution policies are the current foundation for deeper work with
  LLMs, ADTs, type checkers, effect systems, and program verification.
- **Refinement-type verification with Liquid Haskell:** the pure core carries
  machine-checked contracts, verified by SMT at build time and mutation-tested
  so the specs are known to bite. Verified today: the streaming backpressure
  accounting invariant (`Agent.TextBuffer`), terminal grapheme/cursor
  geometry (`Agent.TUI.TextWidth`), fence parsing invariants
  (`Agent.TUI.FencedCode`), composer cursor safety (`Agent.TUI.Model.Edit`,
  `Agent.CLI.Input.EditOps`), column budgets and selection bounds for
  overlays (`Agent.CLI.Input.Display`, `Agent.CLI.TextLayout`), encrypted
  subagent payload non-disclosure through `Show`
  (`Agent.InterAgentMessage`), and workspace path containment
  (`Agent.OsPath`). See [Liquid Haskell verification](#liquid-haskell-verification).

## Features

- **Choice of models and billing:** use OpenAI/Codex, xAI/Grok, OpenRouter,
  Google Gemini, or Claude Code through subscriptions or API keys, and add
  local or hosted Responses-compatible models through the user model catalog.
- **Interactive terminal workflow:** choose between fullscreen and inline
  interfaces with streaming Markdown, live todo progress, one-shot operation,
  and image attachments from files or the clipboard.
- **PostgreSQL-backed memory and portable sessions:** persist conversations and
  scoped learned guidance, resume or search past work, compact long histories,
  and switch supported providers without losing the pending turn or durable
  session state.
- **Efficient long-running agents:** page persisted history on demand and
  virtualize TUI scrolling to bound memory and rendering work as conversations
  grow.
- **Parallel agents and isolated work:** delegate to persisted subagents with a
  configurable concurrency limit and create fresh sessions in managed Git
  worktrees.
- **Built-in coding tools:** run shell commands, opt into a persistent GHCi
  workspace, search the web, and connect local MCP servers. Approval policies
  keep mutating operations under user control.
- **Natural-language Meta Console:** press `Cmd+K` (`Alt+K` on terminals that
  report it that way), or use `/meta <request>`, to preview and apply typed
  model, account, MCP, web-fetch, LSP, shell, and concurrency configuration
  changes without adding the request to the coding conversation.
- **Guided agent workflows:** use plan mode, reusable skills, and scoped learned
  guidance for repeatable tasks and project or user preferences.
- **Multimodal input and live voice dictation:** attach images and files, or
  press `Ctrl+R` on macOS to stream microphone audio to OpenAI or xAI and
  insert the transcript into the prompt.
- **Telegram access:** run a durable, allowlisted Telegram gateway with
  per-conversation sessions, multimodal messages, approvals, retries, and
  bounded concurrent processing.

These are important product features, but not the core differentiation.

## Install

1. Install [GHCup](https://www.haskell.org/ghcup/) (provides GHC and `cabal`),
   then:

   ```console
   git clone https://github.com/overshiki/haskell-agent.git
   cd haskell-agent
   scripts/setup-cabal-build.sh
   cabal install agent-cli:exe:monad-cli
   ```

2. Verify the installation:

   ```console
   monad-cli --help
   ```

The CLI needs PostgreSQL 14 or newer, with the PostgreSQL tools (`initdb`,
`pg_ctl`, `psql`) on `PATH`. Older servers are supported through a
`pgcrypto`-based UUIDv7 fallback, so `pg_catalog.uuidv7()` (PostgreSQL 18+)
is not required. If the tools are installed outside the default `PATH` (for
example in `/usr/lib/postgresql/14/bin`), set `AGENT_POSTGRES_BIN` before
running:

```console
export AGENT_POSTGRES_BIN=/usr/lib/postgresql/14/bin
monad-cli
```

## Run

Start an interactive session:

```console
monad-cli
```

The provider's Bash/shell execution tool is enabled by default. Enable the
persistent `run_ghci` tool when needed:

```console
monad-cli --ghci
```

The GHCi tool is optional and uses a `ghci` executable from `PATH`.

For GHCi-only operation, disable Bash explicitly:

```console
monad-cli --ghci --no-bash
```

During an interactive session, switch the available shell tools without
restarting:

```console
/shell ghci
/shell bash
```

Use `/shell` to show the current selection. `/shell both` and `/shell none`
are also supported.

Run a one-shot task:

```console
monad-cli -p \
  "inspect this Cabal project, explain its architecture, and run its tests"
```

Start in an isolated Git worktree:

```console
monad-cli --worktree
```

By default, managed worktrees fetch and branch from the selected remote's latest
default commit. Repositories without remotes instead branch from the current
local `HEAD`. To disable fetching, add this to `~/.haskell-agent/config.json`:

```json
{
  "version": 1,
  "worktree": {
    "fetchLatestUpstream": false
  }
}
```

This policy applies to `--worktree`, `/worktree`, and subagent worktrees. The
remote is selected from the current branch's configured remote, then
`upstream`, `origin`, or the repository's sole remote. When a remote exists, a
fetch failure aborts worktree creation rather than falling back to a stale
commit.

### Session forks and git branches

Fork the current chat into a new session paired with a git branch, work on the
branch, then merge it back — chat and code stay in step.

```console
/fork --branch my-feature
```

`/fork --branch <name>` creates `my-feature` in the current checkout (from
`HEAD`) and links the new session to it, recording the branch so the session
and the branch move together. Work normally; the agent's edits and commits
land on `my-feature`.

```console
/checkout le
/merge my-feature
```

`/checkout <branch>` switches the git branch and resumes the session linked to
it; `/merge <branch>` merges the branch and appends a turn to the current
session with a model-generated summary of the fork's conversation.

- `/fork` — fork, asking whether to isolate in a worktree or share the checkout.
- `/fork --worktree` / `/fork --no-worktree` — choose the isolation directly.
- `/switch <turn-index>` — rewind the session focus to an earlier turn.
- `/checkout <branch>` — switch branch and resume the chat linked to it.
- `/merge <branch>` — merge the branch and record the fork's conversation.

Sessions record the branch they were created on, so `/checkout` round-trips to
your original session even when the base branch is not `main`. A merge that
conflicts aborts and surfaces git's error for you to resolve in the shell.
See the [forks and checkout guide](docs/session-fork.md) and the
[session tree design](docs/session-tree.md) for the full command surface.

Use `--provider openai`, `--provider xai`, `--provider openrouter`,
`--provider gemini`, or `--provider claude-code` to override automatic
provider detection. Claude Code is selected explicitly rather than by
auto-detection.

Open `/model` and choose a Gemini model such as `gemini-3.7-flash`. If no
Gemini account is connected, the CLI opens Google sign-in in your browser and
stores the OAuth credential in the same managed credential store used by the
other providers:

```console
/model
```

No API key is required for this Google-account flow. For Google AI Studio API
billing instead, set `GOOGLE_API_KEY` (preferred) or `GEMINI_API_KEY`, then run
`monad-cli --provider gemini --model gemini-3.7-flash`.

### Telegram

Create a bot with BotFather, then run:

```console
agent-telegram setup --provider openai --cwd /path/to/project \
  --allowed-user 123456789
agent-telegram start
agent-telegram status
```

The gateway supports durable per-conversation sessions, allowlists,
multimodal messages, approvals, and concurrent chats. See the
[Telegram guide](docs/telegram.md) for setup, groups, and commands.
You can also ask the agent to “set up a Telegram agent”.

### Model catalog and local models

Add local, hosted, or custom models through:

```text
~/.haskell-agent/models.json
```

The built-in `add-model` skill can configure it for you. See the
[model catalog guide](docs/models.md) for the schema, local-server example,
dialects, authentication, and compaction metadata.

The built-in `learn-about-user` skill can derive consent-reviewed technical
defaults from a confirmed public GitHub profile. Invoke it with
`/learn-about-user`, `$learn-about-user`, or a natural-language request.

### Authentication

Works with your Codex, Grok, Google, and Claude accounts, plus provider API
keys. Gemini can be connected interactively from `/model` or `/account`;
`GOOGLE_API_KEY` (or `GEMINI_API_KEY`) remains an optional AI Studio fallback.

### Voice dictation

Press `Ctrl+R` in the prompt composer, speak, and press `Enter` to stop
(or `Esc` to cancel). Recording stays in the TUI; it does not suspend or close
the session. On macOS, the resulting transcript is inserted at the cursor.
Dictation follows the active model provider: OpenAI models use OpenAI and Grok
models use xAI. ChatGPT/Codex OAuth uses the subscription-backed streaming
protocol used by the official desktop app and falls back to its buffered
ChatGPT transcription route with the same recording if streaming fails. API
keys use the public OpenAI Realtime API. Both OpenAI paths can update the
composer while recording. Subscription auth is preferred when both OpenAI
credential types are configured. OpenAI credentials can come from
`CODEX_ACCESS_TOKEN`, `CODEX_AUTH_JSON`, `$CODEX_HOME/auth.json` (defaulting to
`~/.codex/auth.json`),
`OPENAI_API_KEY`, `CODEX_API_KEY`, or a managed OpenAI account.
For Grok models, dictation uses the configured xAI subscription or API-key
credential; set `XAI_STT_LANGUAGE` to override xAI's default `en`.
Dictation is currently unavailable for providers without a speech-to-text
integration.

### Claude Code subscription

Install Claude Code, authenticate it with a first-party Claude subscription,
and select the provider:

```console
claude auth login
monad-cli --provider claude-code --model sonnet
```

The integration keeps a `claude -p` process alive through the reusable
[`claude-agent-sdk-haskell`](packages/claude-agent-sdk-haskell/README.md)
package. Claude Code executes its built-in tools; complementary harness tools
are exposed through an in-process MCP bridge and use the same host approval
policy as other providers. The harness renders events, persists the session,
and performs isolated local-summary compaction before restarting the Claude
continuation. `--yolo` auto-approves ordinary calls, but host catastrophic
command and Plan Mode safeguards remain enforced.

Anthropic's [June 15, 2026 subscription-policy
update](https://support.claude.com/en/articles/15036540-use-the-claude-agent-sdk-with-your-claude-plan)
says that Claude Agent SDK, `claude -p`, and third-party app usage currently
draw from Claude subscription usage limits. Anthropic's current
[Agent SDK documentation](https://platform.claude.com/docs/en/agent-sdk/overview)
also says third-party developers need prior approval to offer Claude.ai login
or subscription rate limits in their products. Technical availability does
not replace that approval requirement; consult the linked documents for
current terms. See [`packages/agent-claude/README.md`](packages/agent-claude/README.md)
for details.

### Local MCP servers

Use `/mcp` to manage local stdio MCP servers, or configure them in
`~/.haskell-agent/config.json`. See the [MCP guide](docs/mcp.md) for the
configuration schema, startup strategies, and tool exposure rules.

### Meta Console

Press `Cmd+K` to open a compact configuration prompt over the current session,
then describe a change such as “add the MCP server at
`https://example.com/mcp`” or “connect my Grok account”. `/meta <request>` is
the keyboard-independent fallback.

Meta Console uses a private, tool-free planner with no coding transcript. Its
typed plan is validated and previewed before execution, and the normal
approval policy still applies. Secrets are requested only through masked
host-owned prompts and are never returned to the planner. See the
[Meta Console guide](docs/meta-console.md) for supported actions and safety
details.

### Secret entry

The built-in `ask_secret` tool reads secrets through a masked prompt and gives
the model only a private temporary-file path, keeping values out of chat and
tool arguments. Files are removed when the tool runtime closes.

### Inline images

The built-in `show_image` tool displays an image file (PNG, JPEG, GIF, BMP,
TIFF) inline in the conversation next to the tool call: Kitty, Ghostty,
WezTerm, and iTerm2 draw the bitmap natively, other terminals get a
true-colour text approximation. The image is shown to the user only; it is
not added to the model context.

## Ideas and direction

Why an independent harness matters, why code and Haskell are useful
foundations, and how types, effects, and verification could make agents safer
are discussed in [`IDEAS.md`](IDEAS.md).

## Architecture

```text
                 monad-cli / future native clients
                              |
                   provider-neutral events
                              |
     +------------------- agent-core -------------------+
     | agent loop | tools | approvals | agents | state |
     +-------------------------+------------------------+
                               |
                    canonical Responses model
                               |
       +---------------+---------------+---------------+---------------+---------------+
       |               |               |               |               |
 agent-openai      agent-xai    agent-openrouter  agent-gemini    agent-claude
       |               |               |               |               |
OpenAI / ChatGPT       xAI          OpenRouter      Gemini         Claude Code
```

The provider-neutral loop sees typed turns, tool calls, tool results, usage,
and streamed events. Provider packages own wire formats, authentication,
transport, and provider-specific continuation. Presentation consumes the same
events through renderer-independent state.

`agent-claude` delegates its generic process transport, protocol decoding, and
session client to
[`claude-agent-sdk-haskell`](packages/claude-agent-sdk-haskell/README.md),
leaving subscription policy and `Agent.Loop` translation in the provider
adapter.

Model targets resolve independently to a provider transport and a model-facing
dialect. OpenAI models use the Codex dialect, xAI models use the Grok Build
dialect, Gemini uses the portable Responses dialect over Google's native Code
Assist or GenerateContent API, and OpenRouter selects Codex, Grok Build, or a
portable Responses dialect from the model family.

## Development

Dependencies come from Hackage via `cabal`. Run `scripts/setup-cabal-build.sh`
once to fetch the patched `vty-unix`, syntax definitions, and bundled model
data, then build and test:

```console
scripts/setup-cabal-build.sh
cabal build agent-cli:exe:monad-cli
cabal test agent-core
```

`scripts/repl` opens the agent under GHCi with automatic reload and session
resume. Edit the harness, leave the running agent (`:q`), and the wrapper
reloads the changed modules and resumes the same session without rebuilding
the executable. It requires `expect`:

```console
scripts/repl
```

The CLI needs PostgreSQL 14 or newer, with the PostgreSQL tools (`initdb`,
`pg_ctl`, `psql`) on `PATH`. Older servers are supported through a
`pgcrypto`-based UUIDv7 fallback, so `pg_catalog.uuidv7()` (PostgreSQL 18+) is
not required. If the tools are installed outside the default `PATH` (for
example in `/usr/lib/postgresql/14/bin`), set `AGENT_POSTGRES_BIN` before
running:

```console
export AGENT_POSTGRES_BIN=/usr/lib/postgresql/14/bin
cabal run agent-cli:exe:monad-cli
```

See [`AGENTS.md`](AGENTS.md) for the complete development workflow, including
multi-package GHCi sessions and CLI testing.

### Liquid Haskell verification

Selected pure modules carry [Liquid
Haskell](https://ucsd-progsys.github.io/liquidhaskell/) refinement annotations
in `{-@ ... @-}` comments. The normal build (any supported GHC) treats them
as comments and is completely unaffected; verification is a separate, opt-in
build that pins the GHC supported by the current Liquid Haskell release
(9.14.1) and needs a Z3 executable on `PATH`:

```console
cabal build --project-file=cabal.liquid.project all
```

A successful run prints `**** LIQUID: SAFE` for each verified module:

- `Agent.TextBuffer` — the buffer's `size`/`count` fields always match the
  retained chunks (the streaming backpressure accounting invariant).
- `Agent.TUI.TextWidth` — grapheme boundary lists start at 0, stay
  non-negative, and never exceed the text length; cursor clamp/next/previous
  motion stays in bounds (includes a structural induction proof).
- `Agent.TUI.FencedCode` — every parsed fenced block carries a valid marker
  (backtick/tilde, length ≥ 3), so renderer and copy-code fence matching
  cannot drift apart.
- `Agent.TUI.Model.Edit` and `Agent.CLI.Input.EditOps` — after every editing
  operation the composer cursor stays inside the text.
- `Agent.CLI.Input.Display` — `takeColumns`/`takeSuffixColumns` never exceed
  the column budget; `Agent.CLI.TextLayout` — a clamped selection index is
  always in range.
- `Agent.InterAgentMessage` — the log-safe `Show` of encrypted subagent
  content is provably constant (`<redacted>`); the payload cannot leak
  through it.
- `Agent.OsPath` — `directoryChain` always ends at the working directory and
  is either the containment fallback or starts at the root;
  `isInsideRelative` never accepts absolute or `".."`-prefixed paths.

Trusted boundaries (small, documented per module): `Text`/`OsPath` are
opaque to the verifier, so a handful of `assume` axioms about them are
trusted; everything else is discharged by Z3. Each module was
mutation-tested — deliberately breaking the implementation fails the
verification build. Layout, trusted-boundary, and extension instructions are
in [`docs/liquid-haskell.md`](docs/liquid-haskell.md).

## License

MIT. See [`LICENSE`](LICENSE).
