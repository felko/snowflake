module Language.Snowflake.Options
    ( runSnowflake
    , module Exports
    ) where

import Language.Snowflake.Options.Parser  as Exports
import Language.Snowflake.Options.Command as Exports
import Language.Snowflake.Options.Types   as Exports

import Options.Applicative

runSnowflake :: IO ()
runSnowflake = execParser (info snowflakeOptions fullDesc) >>= runCommand
