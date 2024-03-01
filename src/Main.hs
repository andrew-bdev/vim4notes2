{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow
import Notebook
import Note


import qualified Monomer.Lens as L
import qualified Notebook as NB
import qualified Note as N
import qualified Monomer.Widgets.Container as Containers
import qualified Monomer.Widgets.Single as Singles
import System.Directory (listDirectory)
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)


data AppModel = AppModel {
  _currentNotebook :: Notebook,
  _currentNote     :: Maybe Note,
  _fileList        :: [FilePath]
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppOpen
  | FileSelected FilePath
  deriving (Eq, Show)

makeLenses 'AppModel

instance TextShow Notebook where
  showb = showb . show

instance TextShow AppEvent where
  showb = showb . show

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Welcome To Noat",
      spacer,
      hstack [
        label $ "Current Notebook: " <> showt (model ^. currentNotebook),
        spacer,
        button "Open Notebook" AppOpen
      ],
      spacer,
      vstack (map (fileButton . FileSelected) (model ^. fileList))
    ] `styleBasic` [padding 10]

fileButton :: AppEvent -> WidgetNode AppModel AppEvent
fileButton evt = button (showt evt) evt

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppOpen -> [Model (model & fileList .~ getFileList)]
  FileSelected filePath -> [Model (loadModelFromFile filePath model)]

getFileList :: [FilePath]
getFileList = unsafePerformIO $ listDirectory "." -- Replace "." with the desired directory path

loadModelFromFile :: FilePath -> AppModel -> AppModel
loadModelFromFile filePath model = model -- Implement loading the notebook from the file

main :: IO ()
main = do
    initNotebook <- loadNotebook "notebook1.txt"
    let initNote = fromMaybe (error "No notes in the notebook!") (NB.getHomeNote initNotebook)
        model = AppModel initNotebook (Just initNote) []
    startApp model handleEvent buildUI config
    where
        config = [
            appWindowTitle "NOAT",
            appWindowIcon "./assets/images/icon.png",
            appTheme darkTheme,
            appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
            appInitEvent AppInit
            ]