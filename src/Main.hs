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
import Debug.Trace

import qualified Data.Map as DM
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
  _currentNote :: Maybe Note,
  _fileList :: [FilePath],
  _textFieldContents :: Text,
  _showFileButtons ::  Bool
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppOpen
  | FileSelected FilePath
  | NotebookLoaded Notebook
  | CreateTestNotebook
  | SaveNotebook
  | AddNote Text
  | NoteSelected Integer
  deriving (Eq, Show)

makeLenses 'AppModel

instance TextShow Notebook where
  showb = showb . show

instance TextShow AppEvent where
  showb = showb . show

buildNoteTree :: AppModel -> Int -> Note -> WidgetNode AppModel AppEvent
buildNoteTree model level note = vstack [
    hstack [
        button "O" (NoteSelected $ N.getNoteId note), -- Add a button for selection
        spacer,
        (if isCurrentNote then labelHighlighted else label) (T.pack $ N.getNoteContent note) `styleBasic` [paddingL (fromIntegral level * 20.0)]
    ],
    vstack $ map (buildNoteTree model (level + 1)) (N.getNoteChildren note)
    ]
  where
    isCurrentNote = Just note == (model ^. currentNote)
    labelHighlighted text = label text `styleBasic` [bgColor (rgb 100 100 100)]

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
          button "Create Test Notebook" CreateTestNotebook,
          spacer,
          button "Save Notebook" SaveNotebook
      ],
      spacer,
      if model ^. showFileButtons
        then scroll (vstack (map (fileButton . FileSelected) (model ^. fileList)))
        else spacer,
      spacer,
      label "Notes:",
      scroll (maybe (label "") (\currentNote -> case getParentNote currentNote (NB.getRootNote =<< model ^. currentNotebook) of
                                                  Just parentNote -> buildNoteTree model 0 parentNote
                                                  Nothing -> label "No parent note found") (model ^. currentNote)),
      hstack [
          textField_ textFieldContents [placeholder "Enter note content here"] `styleBasic` [width 300],
          button "Add Note" (AddNote (model ^. textFieldContents))
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
handleEvent _ _ model evt = case evt of
  AppInit -> handleAppInit model
  AppOpen -> handleAppOpen model
  FileSelected filePath -> handleFileSelected model filePath
  NotebookLoaded notebook -> handleNotebookLoaded model notebook
  CreateTestNotebook -> handleCreateTestNotebook
  AddNote noteContent -> handleAddNote model noteContent
  SaveNotebook -> handleSaveNotebook model
  NoteSelected noteId -> handleNoteSelected model noteId

handleNoteSelected :: AppModel -> Integer -> [AppEventResponse AppModel AppEvent]
handleNoteSelected model noteId = maybe [] setAsCurrentNote (findNote noteId =<< (NB.getRootNote =<< model ^. currentNotebook))
  where
    setAsCurrentNote note = [Model $ trace ("Note selected: " ++ show noteId) (model & currentNote .~ Just note)]

handleSaveNotebook :: AppModel -> [AppEventResponse AppModel AppEvent]
handleSaveNotebook model = maybe [] saveNotebook (model ^. currentNotebook)
  where
    saveNotebook notebook = [Task $ fmap (const AppInit) (NB.saveNotebook notebook "notebook.txt")]

handleAddNote :: AppModel -> Text -> [AppEventResponse AppModel AppEvent]
handleAddNote model noteContent = maybe [] addNote (model ^. currentNote)
  where
    addNote currNote = [Model $ model & currentNote .~ Just (N.addChild (T.unpack noteContent) False currNote)]

handleAppInit :: AppModel -> [AppEventResponse AppModel AppEvent]
handleAppInit _ = []

handleAppOpen :: AppModel -> [AppEventResponse AppModel AppEvent]
handleAppOpen model = [Model (model & fileList .~ getFileList)]

handleFileSelected :: AppModel -> FilePath -> [AppEventResponse AppModel AppEvent]
handleFileSelected _ filePath = [Task $ loadModelFromFile filePath]

handleNotebookLoaded :: AppModel -> Notebook -> [AppEventResponse AppModel AppEvent]
handleNotebookLoaded model notebook = [Model $ model & currentNotebook .~ Just notebook
                                                  & currentNote .~ (NB.getRootNote notebook)]

handleCreateTestNotebook :: [AppEventResponse AppModel AppEvent]
handleCreateTestNotebook = [Task $ do
                              notebook <- NB.createAndSaveNotebook "testNotebook.txt"
                              return $ NotebookLoaded notebook
                            ]

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
    let model = AppModel Nothing Nothing [] "" False
    startApp model handleEvent buildUI config
    where
        config = [
            appWindowTitle "NOAT",
            appWindowIcon "./assets/images/icon.png",
            appTheme darkTheme,
            appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
            appInitEvent AppInit
            ]