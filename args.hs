module Args where

import           Data.List
import           System.Environment

data Args = Args {
  filetypes  :: [String],
    dir      :: String,
    keywords :: [String]
  }deriving (Show)

parse_args :: [String] -> Args -> Args
parse_args (x:y:xs) a
  | x == "-f" || x == "-filetype" = parse_args xs (a {filetypes = y : (filetypes a) })
  | x == "-k" || x == "-keyword" = parse_args xs (a {keywords = y : (keywords a) })
  | x == "-d" || x == "-dir" = parse_args xs (a {dir = y })
  |otherwise = a
parse_args [] a = a


init_args :: Args
init_args = Args [] "" []
