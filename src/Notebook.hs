{-# LANGUAGE TemplateHaskell #-}

module Notebook
  ( Notebook(..)
  , loadNotebook
  , saveNotebook
  , createAndSaveNotebook
  , getNotes
  , newNotebook
  , getRootNote
  , createNote
  , getName
  ) where

import Control.Exception
import Control.Lens
import System.IO
import System.IO.Error
import Note

data Notebook = Notebook {
        _title :: String,
        _notes :: [Note]
} deriving (Show, Read, Eq)

makeLenses ''Notebook

getRootNote :: Notebook -> Maybe Note
getRootNote notebook = case getNotes notebook of
    [] -> Nothing
    (x:_) -> Just x

getName :: Notebook -> String
getName = view title

newNotebook :: String -> [Note] -> Notebook
newNotebook = Notebook

getNotes :: Notebook -> [Note]
getNotes = view notes

addNote :: Note -> Notebook -> Notebook
addNote note notebook = notebook & notes %~ (note :)

loadNotebook :: FilePath -> IO (Either String Notebook)
loadNotebook filePath = do
    result <- try $ readFile filePath
    return $ case result of
        Left ex -> Left $ "Failed to read file: " ++ show (ex :: IOException)
        Right content -> case reads content of
            [(notebook, "")] -> Right notebook
            _ -> Left $ "Failed to parse notebook from content: " ++ content
            
createAndSaveNotebook :: FilePath -> IO Notebook
createAndSaveNotebook filename = do
    let welcomeNote = Note 1 "Welcome to vim4notes" True 
                        [ Note 2 "This is for notetaking" False []
                        , Note 3 "You can have subnotes" False []
                        , Note 4 "And more stuff" False []
                        ]
    let notebook = newNotebook "My New Notebook" [welcomeNote]
    saveNotebook notebook filename
    return notebook

saveNotebook :: Notebook -> FilePath -> IO ()
saveNotebook notebook filename = writeFile filename (show notebook)