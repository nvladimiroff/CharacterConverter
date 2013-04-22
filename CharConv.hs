module Main
    where

import System.Environment

main = do
    args <- getArgs
    case args of
        "fullwidth":xs      -> putStrLn $ convert fullwidth $ unwords xs
        "boldmath":xs       -> putStrLn $ convert boldmath $ unwords xs
        "italicmath":xs     -> putStrLn $ convert italicmath $ unwords xs
        "bolditalicmath":xs -> putStrLn $ convert bolditalicmath $ unwords xs
        "script":xs         -> putStrLn $ convert script $ unwords xs
        _                   -> putStrLn help

help = "Converts regular characters into another equivalent character set\n" ++
       "Supported sets:\n" ++
       "    fullwidth, boldmath, italicmath\n" ++
       "Usage: charconv fullwidth hello world\n" ++
       "Nothing after the character set will default"

alphabet = ['A' .. 'Z'] ++ ['a' .. 'z']

fullwidth = zip (" !\"#$%&'()*+,-./0123456789:;<=>?@" ++ ['A' .. 'Z'] ++ "[\\]^_`" ++ ['a' .. 'z'] ++ "{|}~")
                ('\x3000' : ['\xFF01' .. '\xFF5E'])

boldmath = zip alphabet
               ['\x1D400' .. '\x1D433']

-- Lower case h is already implemented as 201E
italicmath = zip alphabet
                 (['\x1D434' .. '\x1D454'] ++ "\x210E" ++ ['\x1D456' .. '\x1D467'])

bolditalicmath = zip alphabet
                     ['\x1D468' .. '\x1D49B']

-- Tons of characters are encoded in different blocks.
script = zip alphabet
             ("\x1D49C\x212C\x1D49E\x1D49F\x2130\x2131\x1D4A2\x210B\x2110\x1D4A5" ++
              "\x1D4A6\x2112\x2133" ++ ['\x1D4A9' .. '\x1D4AC'] ++ "\x211B" ++
              ['\x1D4AE' .. '\x1D4B9'] ++ "\x212F\x1D4BB\x210A" ++
              ['\x1D4BD' .. '\x1D4C3'] ++ "\x2134" ++ ['\x1D4C5' .. '\x1D4CF'])

convert :: [(Char,Char)] -> [Char] -> [Char]
convert cset text = map (\x -> case lookup x cset of
                                Just y -> y
                                Nothing -> x) text
