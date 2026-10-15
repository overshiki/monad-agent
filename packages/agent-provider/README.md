# agent-provider

Provider clients for the [haskell-agent](../../README.md) harness, merged into a
single package with one internal **sublibrary per provider**. Module names are
unchanged from the former standalone packages.

| Sublibrary | Modules | Former package |
|------------|---------|----------------|
| `openai`   | `Agent.OpenAI.*`   | `agent-openai` (incl. vendored ChatGPT Codex transport, see `CHANGELOG.md` / `UPSTREAM.md`) |
| `xai`      | `Agent.XAI.*`      | `agent-xai` |
| `openrouter` | `Agent.OpenRouter.*` | `agent-openrouter` |
| `gemini`   | `Agent.Gemini.*`   | `agent-gemini` |
| `deepseek` | `Agent.DeepSeek.*` | `agent-deepseek` |
| `kimi`     | `Agent.Kimi.*`     | `agent-kimi` |
| `kimi-code` | `Agent.KimiCode.*` | `agent-kimi-code` |
| `claude`   | `Agent.Claude.*`   | `agent-claude` |

## Using it

Depend on only the sublibraries you need:

```cabal
build-depends: agent-provider:{openai, deepseek} >= 0.1 && < 0.2
```

Each sublibrary compiles independently, so building one provider does not
recompile the others.

## Layout

- `src/Agent/<Provider>/` — provider client sources (shared source dir, one
  module tree per sublibrary)
- `test/Agent/<Provider>/` + `test-mains/<provider>/Spec.hs` — hspec suites,
  one test component per provider (`openai-test`, `deepseek-test`, ...)
- `app/` — `openai-login` executable (Codex device-code login)
- `benchmark/` — `codex-http-streaming-bench`
- `data/` — Codex bundled data fetched by `scripts/setup-cabal-build.sh`
  (gitignored, not vendored)

## Tests

```console
cabal test agent-provider:test:deepseek-test
```
