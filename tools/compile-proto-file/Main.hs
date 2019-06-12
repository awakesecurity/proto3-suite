{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

import           Options.Generic
import           Prelude                        hiding (FilePath)
import           Proto3.Suite.DotProto.Generate
import           Turtle                         (FilePath)

main :: IO ()
main =
  unwrapRecord "Compiles a .proto file to a Haskell module"
  >>= compileDotProtoFileOrDie
