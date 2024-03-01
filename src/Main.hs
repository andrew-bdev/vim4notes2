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
import Control.Exception

import qualified Monomer.Lens as L
import qualified Notebook as NB
import qualified Note as N
import qualified Monomer.Widgets.Container as Containers
import qualified Monomer.Widgets.Single as Singles
import qualified Data.Text as T
import System.Directory (listDirectory)
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isSuffixOf)

data AppModel = AppModel {
  _currentNotebook :: Maybe Notebook,
  _currentNote     :: Maybe Note,
  _fileList        :: [FilePath],
  _newNoteText     :: Text
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppOpen
  | FileSelected FilePath
  | NotebookLoaded Notebook
  | CreateTestNotebook
  | NewNoteEntered Text
  deriving (Eq, Show)

makeLenses 'AppModel

instance TextShow Notebook where
  showb = showb . show

instance TextShow AppEvent where
  showb = showb . show

buildNoteTree :: AppModel -> Int -> Note -> WidgetNode AppModel AppEvent
buildNoteTree model level note = vstack [
    label (T.pack $ N.getNoteContent note) `styleBasic` (paddingL (fromIntegral level * 10.0) : if Just note == (model ^. currentNote) then [textColor (rgb 0 0 255)] else []),
    vstack $ map (buildNoteTree model (level + 1)) (N.getNoteChildren note)
  ]

-- Modify the call to buildNoteTree in buildUI function
buildUI :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Welcome To Noat",
      spacer,
      hstack [
          label $ "Current Notebook: " <> getNotebookName model,
          spacer,
          button "Open Notebook" AppOpen,
          spacer,
          button "Create Test Notebook" CreateTestNotebook
      ],
      spacer,
      scroll (vstack (map (fileButton . FileSelected) (model ^. fileList))),
      spacer,
      label "Notes:",
      scroll (maybe (label "") (buildNoteTree model 0) (model ^. currentNote)),
      spacer,
      hstack [
          textField (newNoteText . L.text) `styleBasic` [width 300],
          button "Add Note" (NewNoteEntered (model ^. newNoteText))
      ]
    ] `styleBasic` [padding 10]

fileButton :: AppEvent -> WidgetNode AppModel AppEvent
fileButton evt = case evt of
  FileSelected filePath -> button (T.pack filePath) evt
  _ -> button (showt evt) evt

getNoteContents :: AppModel -> [Text]
getNoteContents model = maybe [] (map (T.pack . N.getNoteContent) . NB.getNotes) (model ^. currentNotebook)

getNotebookName :: AppModel -> Text
getNotebookName model = maybe "No Notebook" (T.pack . NB.getName) (model ^. currentNotebook)

handleEvent :: WidgetEnv AppModel AppEvent -> WidgetNode AppModel AppEvent -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppOpen -> [Model (model & fileList .~ getFileList)]
  FileSelected filePath -> [Task $ loadModelFromFile filePath]
  NotebookLoaded notebook -> [Model $ model & currentNotebook .~ Just notebook
                                            & currentNote .~ Just (NB.getRootNote notebook)]
  CreateTestNotebook -> [Task $ do
                          notebook <- NB.createAndSaveNotebook "testNotebook.txt"
                          return $ NotebookLoaded notebook
                        ]
  NewNoteEntered text -> [Task $ do
      -- Add the new note to the current note as a child
      let updatedNote = N.addChild (T.unpack text) True (model ^. currentNote)
      -- Update the current note in the model
      let updatedModel = model & currentNote .~ Just updatedNote
      -- Reload the tree
      return $ Model updatedModel, Update]

loadModelFromFile :: FilePath -> IO AppEvent
loadModelFromFile filePath = do
  result <- try $ NB.loadNotebook filePath
  case result of
    Left ex -> do
      putStrLn $ "Failed to load notebook: " ++ show (ex :: IOException)
      return AppInit -- Return a default event if loading fails
    Right (Left errMsg) -> do
      putStrLn $ "Failed to parse notebook: " ++ errMsg ++ " for file: " ++ filePath
      return AppInit -- Return a default event if parsing fails
    Right (Right notebook) -> return $ NotebookLoaded notebook

getFileList :: [FilePath]
getFileList = unsafePerformIO $ do
    files <- listDirectory "." -- Replace "." with the desired directory path
    let txtFiles = filter (".txt" `isSuffixOf`) files
    print txtFiles -- Debugging line
    return txtFiles

main :: IO ()
main = do
    let model = AppModel Nothing Nothing [] ""
    startApp model handleEvent buildUI config
    where
        config = [
            appWindowTitle "NOAT",
            appWindowIcon "./assets/images/icon.png",
            appTheme darkTheme,
            appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
            appInitEvent AppInit
            ]