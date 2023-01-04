{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Resource.Models where

import Data.Text
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Status of artifact
data ArtifactStatus = Achieved | Desired | Irrelevant
  deriving stock (Generic, Show, Read, Eq)

derivePersistField "ArtifactStatus"

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

Target
  parent TargetId Maybe
  description Text
  priority Int

Artifact
  creator EmployeeId
  responsible EmployeeId
  createdAt UTCTime
  deliveredAt UTCTime Maybe
  description Text
  result Text Maybe
  status ArtifactStatus
  parent TargetId Maybe
  priority Int

Employee
  name Text
|]
