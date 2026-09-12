module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.KimiCode.RequestSpec as RequestSpec
import qualified Agent.KimiCode.StreamSpec as StreamSpec
import qualified Agent.KimiCode.CredentialSpec as CredentialSpec
import qualified Agent.KimiCode.ErrorSpec as ErrorSpec

main :: IO ()
main = hspec do
    RequestSpec.spec
    StreamSpec.spec
    CredentialSpec.spec
    ErrorSpec.spec
