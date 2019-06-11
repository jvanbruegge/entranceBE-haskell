{-# LANGUAGE TemplateHaskell #-}

module MongoSettings (mongoSettings) where

import Language.Haskell.TH (Type(ConT))
import Database.Persist.TH (MkPersistSettings, mkPersistSettings, mpsGeneric)
import Database.Persist.MongoDB (MongoContext)

mongoSettings :: MkPersistSettings
mongoSettings =
    (mkPersistSettings (ConT ''MongoContext))
    { mpsGeneric = False }
