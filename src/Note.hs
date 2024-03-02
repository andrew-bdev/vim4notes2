module Note 
    ( Note(..)
    , loadNote
    , saveNote
    , newNote
    , addChild
    , removeChild
    , printNote
    , getNoteContent
    , getNoteChildren
    , getNoteId
    , updateContent
    , createNote
    , findNote
    , getParentNote
    ) where

import System.IO
import System.IO.Error
import System.Directory
import System.Environment
import Data.Time.Clock.POSIX
import Control.Monad (mplus)
import Data.Maybe (listToMaybe, catMaybes)

data Note = Note {
    noteid :: Integer,
    content :: String,
    section :: Bool,
    children :: [Note]
} deriving (Show, Read, Eq)

isSection :: Note -> Bool
isSection = section

updateContent :: String -> Note -> Note
updateContent newContent note = note { content = newContent }

getNoteChildren :: Note -> [Note]
getNoteChildren = children

getNoteId :: Note -> Integer
getNoteId = noteid

createNote :: String -> Bool -> [Note] -> Note
createNote contentRequest typeRequest childrenRequest = Note {
    noteid = 0,
    content = contentRequest,
    section = typeRequest,
    children = childrenRequest
}

loadNote :: FilePath -> IO Note
loadNote filename = read <$> filter (/= '\n') <$> readFile filename

saveNote :: Note -> FilePath -> IO ()
saveNote note filename = writeFile filename (show note)

getParentNote :: Note -> Maybe Note -> Maybe Note
getParentNote targetNote currentNote = case currentNote of
    Nothing -> Nothing
    Just note -> if any (\child -> getNoteId child == getNoteId targetNote) (getNoteChildren note)
        then Just note
        else (Just note) `mplus` (listToMaybe $ catMaybes $ map (getParentNote targetNote . Just) (getNoteChildren note))

findNote :: Integer -> Note -> Maybe Note
findNote noteId note =
    if noteid note == noteId
    then Just note
    else case children note of
        [] -> Nothing
        _ -> foldl (\acc child -> acc `mplus` findNote noteId child) Nothing (children note)

newNote :: String -> Bool -> Note
newNote contentRequest typeRequest = Note {
    noteid = 0,
    content = contentRequest,
    section = typeRequest,
    children = []
}

addChild :: String -> Bool -> Note -> Note
addChild contentRequest typeRequest parentNote =
    let child = Note {
        noteid = noteid parentNote, 
        content = contentRequest,
        section = typeRequest,
        children = []
    }
    in parentNote { children = children parentNote ++ [child] }

getNoteContent :: Note -> String
getNoteContent = content

removeChild :: Integer -> Note -> Note
removeChild removeRequestNoteId parentNote =
    let updatedChildren = filter (\child -> noteid child /= removeRequestNoteId) (children parentNote)
    in parentNote { children = updatedChildren }

printNote :: Int -> Maybe Note -> Note -> String
printNote level current note =
    let formattedContent = if section note
                           then replicate level ' ' ++ makeUnderline (content note)
                           else replicate level ' ' ++ "- " ++ content note
        highlightedContent = if Just note == current
                             then "\x1b[31m" ++ formattedContent ++ "\x1b[0m"
                             else formattedContent
        result = highlightedContent ++ "\n" ++ concatMap (printNote (level + 1) current) (children note)
    in result

makeUnderline :: String -> String
makeUnderline str = "\x1B[4m" ++ str ++ "\x1B[0m"