module Proto3.Suite.DotProto.Generate.Swagger.OverrideToSchema
  ( OverrideToSchema(..)
  ) where

{-| This is a hack to work around the `swagger2` library forbidding `ToSchema`
    instances for `ByteString`s
-}
newtype OverrideToSchema a = OverrideToSchema { unOverride :: a }
