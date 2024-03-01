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
    ) where

import System.IO
import System.IO.Error
import System.Directory
import System.Environment
import Data.Time.Clock.POSIX 

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