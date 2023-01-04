{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Resource.Lib where

import Brick (App (..), AttrMap, BrickEvent (VtyEvent), Padding (Pad), Widget, attrMap, customMain, hLimit, halt, on, padTop, str)
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form (formFocus, formState), editTextField, focusedFormInputAttr, handleFormEvent, invalidFormInputAttr, newForm, renderForm, (@@=))
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlite)
import qualified Graphics.Vty as V
import Lens.Micro.TH
import Resource.Models (migrateAll)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))

data EmployeeView = EmployeeView
  { _name :: Text
  }
  deriving (Show)

makeLenses ''EmployeeView

data ArtifactStatusView = Achieved | Desired | Irrelevant
  deriving stock (Show, Read, Eq)

data ArtifactView = ArtifactView
  { _creator :: EmployeeView
  , _responsible :: EmployeeView
  , _description :: Text
  , _result :: Maybe Text
  , _status :: ArtifactStatusView
  , _createdAt :: UTCTime
  , _deliveredAt :: Maybe UTCTime
  , _priority :: Int
  }
  deriving (Show)

makeLenses ''ArtifactView

data TargetView = TargetView
  { _artifacts :: [ArtifactView]
  , _subTargets :: [TargetView]
  , _priority :: Int
  }
  deriving (Show)

mkArtifactForm :: ArtifactView -> Form ArtifactView e ()
mkArtifactForm =
  newForm
    [ B.borderWithLabel (str "Ответственный")
        @@= editTextField (responsible . name) () Nothing
    ]

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.black)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    , (invalidFormInputAttr, V.white `on` V.red)
    , (focusedFormInputAttr, V.black `on` V.yellow)
    ]

drawArtifact :: Form ArtifactView e () -> [Widget ()]
drawArtifact f = [C.vCenter $ C.hCenter form]
 where
  form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f

app :: App (Form ArtifactView e ()) e ()
app =
  App
    { appDraw = drawArtifact
    , appHandleEvent = \ev -> do
        case ev of
          VtyEvent (V.EvResize{}) -> return ()
          VtyEvent (V.EvKey V.KEsc []) -> halt
          _ -> handleFormEvent ev
    , appChooseCursor = focusRingCursor formFocus
    , appStartEvent = return ()
    , appAttrMap = const theMap
    }

run :: IO ()
run = do
  home <- getHomeDirectory
  let appDir = home </> ".local/share/Resource"
      sqliteFileName = "db"
      sqliteFilePath = appDir </> sqliteFileName
  createDirectoryIfMissing True appDir
  runSqlite (Text.pack sqliteFilePath) $ do
    runMigration migrateAll
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

      initialUserInfo =
        ArtifactView
          { _creator = EmployeeView{_name = "creator"}
          , _responsible = EmployeeView{_name = "responsible"}
          , _description = "description"
          , _result = Nothing
          , _status = Desired
          , _createdAt = undefined
          , _deliveredAt = Nothing
          , _priority = 0
          }
      f = mkArtifactForm initialUserInfo

  initialVty <- buildVty
  f' <- customMain initialVty buildVty Nothing app f
  print $ formState f'