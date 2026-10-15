-- Compat shim: filepath >= 1.4.3 (GHC 9.6.7 boot library) no longer exports
-- @unsafeEncodeUtf@ from "System.OsPath". Sources that predate that change
-- (the openai-login executable and Agent.OpenAI.LoginSpec) still import it,
-- so recover the symbol here and re-export the rest of filepath's module.
-- Same compat that agent-core's Agent.OsPath provides for library code; this
-- copy exists because cabal components cannot share modules by dependency
-- without exposing the shim package-wide. filepath's original module is
-- brought in as RawOsPath via the components' mixins field.
module System.OsPath
    ( OsPath
    , (</>)
    , decodeFS
    , unsafeEncodeUtf
    ) where

import RawOsPath (OsPath, decodeFS, (</>))
import qualified RawOsPath as Raw
import Control.Exception (throw)

-- | Compatibility alias for code written against older @filepath@ versions
-- that exported @unsafeEncodeUtf@ from "System.OsPath".
unsafeEncodeUtf :: String -> OsPath
unsafeEncodeUtf = either throw id . Raw.encodeUtf
