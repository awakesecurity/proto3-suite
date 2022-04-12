module Proto3.Suite.DotProto.Generate.Swagger
  ( OverrideToSchema (..)
  )
where

newtype OverrideToSchema a = OverrideToSchema { unOverride :: a }
