module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.Kimi.ClientSpec as ClientSpec
import qualified Agent.Kimi.CredentialSpec as CredentialSpec
import qualified Agent.Kimi.ErrorSpec as ErrorSpec
import qualified Agent.Kimi.RequestSpec as RequestSpec
import qualified Agent.Kimi.UsageSpec as UsageSpec

main :: IO ()
main = hspec do
    RequestSpec.spec
    ErrorSpec.spec
    CredentialSpec.spec
    ClientSpec.spec
    UsageSpec.spec
