module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.DeepSeek.ClientSpec as ClientSpec
import qualified Agent.DeepSeek.CredentialSpec as CredentialSpec
import qualified Agent.DeepSeek.ErrorSpec as ErrorSpec
import qualified Agent.DeepSeek.RequestSpec as RequestSpec
import qualified Agent.DeepSeek.ThinkingModeContractSpec as ThinkingModeContractSpec
import qualified Agent.DeepSeek.UsageSpec as UsageSpec

main :: IO ()
main = hspec do
    RequestSpec.spec
    ThinkingModeContractSpec.spec
    ErrorSpec.spec
    CredentialSpec.spec
    ClientSpec.spec
    UsageSpec.spec
