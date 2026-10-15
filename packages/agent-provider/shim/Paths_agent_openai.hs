-- Compatibility shim: the vendored Codex transport imports @Paths_agent_openai@
-- for the package version. After the provider merge the autogen module is
-- @Paths_agent_provider@ (one per package, not per sublibrary), so re-export
-- it under the old name. Version value is unchanged (0.1.0.0).
module Paths_agent_openai (version) where

import Paths_agent_provider (version)
