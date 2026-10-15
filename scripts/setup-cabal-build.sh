#!/usr/bin/env bash
# Setup a pure-Cabal build environment for haskell-agent.
#
# This script sets up the runtime data and patched dependency the builds and
# tests need (the project has no Nix support). It is intentionally
# conservative: it checks prerequisites, downloads vendored files, patches
# vty-unix, and writes a cabal.project.local. It does not run sudo commands
# automatically.
#
# Usage:
#   scripts/setup-cabal-build.sh
#   scripts/setup-cabal-build.sh --build
#
# Environment overrides:
#   VTY_UNIX_VERSION      vty-unix version to patch (default: 0.3.0.0)
#   SKYLIGHTING_REV       skylighting commit to fetch (default: e432d65743ecef9475816b2cc074d34833837ced)
#   CODEX_REV             openai/codex commit for bundled data (default: 4f39251a010a8bd7d692d25fb33832ff06f1635a)
#   WORK_DIR              where aux files are kept (default: ROOT/.cabal-build-aux)

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

BUILD="${BUILD:-0}"
for arg in "$@"; do
    case "$arg" in
        --build) BUILD=1 ;;
        -h|--help)
            sed -n '2,17p' "$0"
            exit 0
            ;;
        *) die "unknown argument: $arg (try --help)" ;;
    esac
done

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
VTY_UNIX_VERSION="${VTY_UNIX_VERSION:-0.3.0.0}"
SKYLIGHTING_REV="${SKYLIGHTING_REV:-e432d65743ecef9475816b2cc074d34833837ced}"
CODEX_REV="${CODEX_REV:-4f39251a010a8bd7d692d25fb33832ff06f1635a}"
WORK_DIR="${WORK_DIR:-$ROOT/.cabal-build-aux}"

mkdir -p "$WORK_DIR"

log() { echo "[setup-cabal-build] $*"; }
die() { echo "[setup-cabal-build] ERROR: $*" >&2; exit 1; }

# ---------------------------------------------------------------------------
# 1. Check toolchain
# ---------------------------------------------------------------------------
command -v ghc >/dev/null 2>&1 || die "ghc not found in PATH"
command -v cabal >/dev/null 2>&1 || die "cabal not found in PATH"

GHC_VERSION="$(ghc --numeric-version)"
log "GHC version: $GHC_VERSION"

# The CLI package requires base >= 4.17 (GHC 9.4). GHC 9.10.x is the
# recommended version for best compatibility.
case "$GHC_VERSION" in
    9.10.*) ;;
    9.8.*|9.6.*|9.4.*)
        log "WARNING: GHC $GHC_VERSION may work but GHC 9.10.x is the recommended version."
        ;;
    *)
        die "GHC $GHC_VERSION is too old or untested. Install GHC 9.10.x."
        ;;
esac

# ---------------------------------------------------------------------------
# 2. Check runtime tools
# ---------------------------------------------------------------------------
check_tool() {
    local name="$1"
    local purpose="$2"
    if command -v "$name" >/dev/null 2>&1; then
        log "found $name: $(command -v "$name")"
    else
        log "WARNING: $name not found. $purpose"
    fi
}

check_tool git "required for worktrees and project context"
check_tool rg "required for code-search tools"
check_tool ffmpeg "required for voice dictation"
check_tool bun "required for the CodeMode JS worker (must be 1.4.x)"
check_tool psql "PostgreSQL client; set AGENT_POSTGRES_BIN to its bin directory"
check_tool pg_ctl "PostgreSQL server control; set AGENT_POSTGRES_BIN to its bin directory"
check_tool pg_config "PostgreSQL build config; libpq headers must be installed"

if command -v bun >/dev/null 2>&1; then
    BUN_VERSION="$(bun --version 2>/dev/null || true)"
    case "$BUN_VERSION" in
        1.4.*) ;;
        *) log "WARNING: bun version is '$BUN_VERSION'; the project pins 1.4.x." ;;
    esac
fi

# ---------------------------------------------------------------------------
# 3. Check system libraries
# ---------------------------------------------------------------------------
if command -v pkg-config >/dev/null 2>&1; then
    for lib in libpq openssl libffi zlib ncursesw gmp; do
        if pkg-config --exists "$lib" 2>/dev/null; then
            log "pkg-config: $lib found"
        else
            log "WARNING: pkg-config cannot find '$lib'. Install the -dev package for your distro."
        fi
    done
else
    log "WARNING: pkg-config not found; cannot verify system libraries."
fi

# ---------------------------------------------------------------------------
# 4. Fetch skylighting syntax definitions
# ---------------------------------------------------------------------------
SKYLIGHTING_DIR="$WORK_DIR/skylighting-$SKYLIGHTING_REV"
SKYLIGHTING_XML_DIR="$SKYLIGHTING_DIR/skylighting-core/xml"

if [ -d "$SKYLIGHTING_XML_DIR" ] && [ -f "$SKYLIGHTING_XML_DIR/haskell.xml" ]; then
    log "skylighting syntax defs already present at $SKYLIGHTING_DIR"
else
    log "fetching skylighting $SKYLIGHTING_REV ..."
    rm -rf "$SKYLIGHTING_DIR"
    git clone --quiet --depth 1 "https://github.com/jgm/skylighting.git" "$SKYLIGHTING_DIR"
    (cd "$SKYLIGHTING_DIR" && git fetch --quiet --depth 1 origin "$SKYLIGHTING_REV" && git checkout --quiet FETCH_HEAD)
fi

export AGENT_SYNTAX_DIR="$SKYLIGHTING_XML_DIR"

# ---------------------------------------------------------------------------
# 5. Fetch Codex bundled data for agent-provider
# ---------------------------------------------------------------------------
DATA_DIR="$ROOT/packages/agent-provider/data"
mkdir -p "$DATA_DIR"

fetch_codex_data() {
    local file="$1"
    local raw_url="https://raw.githubusercontent.com/openai/codex/$CODEX_REV/codex-rs/models-manager/$file"
    local api_url="https://api.github.com/repos/openai/codex/contents/codex-rs/models-manager/$file?ref=$CODEX_REV"
    local dest="$DATA_DIR/$file"

    if [ -s "$dest" ]; then
        log "agent-provider/data/$file already present"
        return 0
    fi

    log "downloading $file from openai/codex $CODEX_REV ..."
    rm -f "$dest"

    # Try raw.githubusercontent.com first (fastest, no rate limit).
    if command -v curl >/dev/null 2>&1; then
        if curl -fsSL --http1.1 --retry 3 --retry-delay 2 -o "$dest" "$raw_url" 2>/dev/null; then
            return 0
        fi
        log "raw download failed for $file, trying GitHub API ..."
    elif command -v wget >/dev/null 2>&1; then
        if wget -q -O "$dest" "$raw_url" 2>/dev/null; then
            return 0
        fi
        log "raw download failed for $file, trying GitHub API ..."
    fi

    # Fall back to the GitHub API (base64-encoded content). This is more
    # reliable on networks where raw.githubusercontent.com is unstable.
    if command -v python3 >/dev/null 2>&1; then
        python3 - "$api_url" "$dest" <<'PY'
import base64, json, sys, urllib.request
url, dest = sys.argv[1], sys.argv[2]
req = urllib.request.Request(url, headers={"User-Agent": "haskell-agent-setup"})
last_err = None
for attempt in range(5):
    try:
        with urllib.request.urlopen(req, timeout=60) as resp:
            data = json.loads(resp.read())
        content = base64.b64decode(data["content"])
        with open(dest, "wb") as f:
            f.write(content)
        sys.exit(0)
    except Exception as e:
        last_err = e
if last_err:
    print(f"GitHub API download failed: {last_err}", file=sys.stderr)
sys.exit(1)
PY
        return 0
    fi

    die "could not download $file (tried curl/wget/raw and GitHub API). Install curl, wget, or python3."
}

fetch_codex_data models.json
fetch_codex_data prompt.md

# ---------------------------------------------------------------------------
# 6. Patch vty-unix locally
# ---------------------------------------------------------------------------
VTY_UNIX_DIR="$WORK_DIR/vty-unix-$VTY_UNIX_VERSION"

if [ -d "$VTY_UNIX_DIR" ]; then
    log "vty-unix $VTY_UNIX_VERSION already fetched"
else
    log "fetching vty-unix $VTY_UNIX_VERSION ..."
    cabal get --destdir="$WORK_DIR" "vty-unix-$VTY_UNIX_VERSION"
fi

PATCH_MARKER="$VTY_UNIX_DIR/.haskell-agent-patch-applied"
if [ -f "$PATCH_MARKER" ]; then
    log "vty-unix patch already applied"
else
    log "applying vty-unix-all-motion.patch ..."
    if ! patch -p1 -d "$VTY_UNIX_DIR" -i "$ROOT/patches/vty-unix-all-motion.patch" --dry-run >/dev/null 2>&1; then
        die "patch does not apply to vty-unix-$VTY_UNIX_VERSION. Override VTY_UNIX_VERSION or patch manually."
    fi
    patch -p1 -d "$VTY_UNIX_DIR" -i "$ROOT/patches/vty-unix-all-motion.patch"
    touch "$PATCH_MARKER"
fi

# cabal's source-repository-package needs a real git repository with a commit
# we can point to. Initialise one in the patched tree.
VTY_UNIX_TAG=""
if [ -d "$VTY_UNIX_DIR/.git" ]; then
    VTY_UNIX_TAG="$(cd "$VTY_UNIX_DIR" && git rev-parse HEAD)"
else
    log "initialising git repo in patched vty-unix tree ..."
    (cd "$VTY_UNIX_DIR" && git init -q && git add -A && git -c user.email=setup@cabal-build -c user.name=Setup commit -q -m "patched vty-unix")
    VTY_UNIX_TAG="$(cd "$VTY_UNIX_DIR" && git rev-parse HEAD)"
fi

# ---------------------------------------------------------------------------
# 7. Write cabal.project.local
# ---------------------------------------------------------------------------
LOCAL="$ROOT/cabal.project.local"

if [ -f "$LOCAL" ]; then
    BACKUP="$LOCAL.$(date +%Y%m%d-%H%M%S).bak"
    cp "$LOCAL" "$BACKUP"
    log "backed up existing cabal.project.local to $BACKUP"
fi

{
    echo "-- Generated by scripts/setup-cabal-build.sh"
    echo "-- Edit at your own risk, or delete and re-run the script."
    echo
    echo "-- Use the locally patched vty-unix instead of the Hackage version."
    echo "source-repository-package"
    echo "    type: git"
    echo "    location: file://$VTY_UNIX_DIR"
    echo "    tag: $VTY_UNIX_TAG"
    echo
    echo "-- Reinforce the Hasql pin from cabal.project to avoid solver drift"
    echo "-- when the local Hackage index has newer versions."
    echo "constraints:"
    echo "    hasql == 2.0.1.0,"
    echo "    hasql-pool == 1.5.0.1,"
    echo "    hasql-transaction == 1.2.3.1"
} > "$LOCAL"

log "wrote $LOCAL"

# ---------------------------------------------------------------------------
# 8. Update cabal package index
# ---------------------------------------------------------------------------
log "running cabal update ..."
cabal update

# ---------------------------------------------------------------------------
# 9. Optional build
# ---------------------------------------------------------------------------
if [ "$BUILD" -eq 1 ]; then
    log "building agent-cli:exe:monad-cli ..."
    cabal build agent-cli:exe:monad-cli
else
    log "skipping build (pass --build to build now)"
fi

# ---------------------------------------------------------------------------
# 10. Summary
# ---------------------------------------------------------------------------
cat <<EOF

----------------------------------------------------------------------
Pure-Cabal environment ready.

Required environment variables for running the agent:

  export AGENT_SYNTAX_DIR="$AGENT_SYNTAX_DIR"
  export AGENT_POSTGRES_BIN="<path-to-postgres-bin-dir>"

Optional but recommended:

  export PATH="<path-to-bun-1.4>:<path-to-ffmpeg>:<path-to-rg>:\$PATH"

Build the CLI with:

  cabal build agent-cli:exe:monad-cli

Run the REPL with:

  cabal repl agent-cli
  -- then in GHCi:
  -- import System.Environment (withArgs)
  -- withArgs ["--worktree"] run

Run tests with:

  export AGENT_SYNTAX_DIR="$AGENT_SYNTAX_DIR"
  export AGENT_POSTGRES_BIN="<path-to-postgres-bin-dir>"
  cabal test all
----------------------------------------------------------------------
EOF
