module Main where

import Text.Pandoc.SideNote (usingSideNotes)
import Text.Pandoc.JSON (toJSONFilter)

main :: IO ()
main = toJSONFilter usingSideNotes
