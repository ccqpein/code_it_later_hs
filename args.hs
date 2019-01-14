module Args where

import qualified Data.ByteString.Lazy.Char8 as BL

data Args = Args {
  filetypes  :: [String],
    dir      :: String,
    keywords :: [String]
  } deriving (Show,Eq)

parse_args :: [String] -> Args -> Args
parse_args [] a = a
parse_args (x:y:xs) a
  | x == "-f" || x == "-filetype" = parse_args xs (a {filetypes = (map ("." ++) (handle_mutil_str y)) ++ (filetypes a) })
  | x == "-k" || x == "-keyword" = parse_args xs (a {keywords = (handle_mutil_str y) ++ (keywords a) })
  | x == "-d" || x == "-dir" = parse_args xs (a {dir = y })
  |otherwise = a



handle_mutil_str :: [Char] -> [[Char]]
handle_mutil_str = (filter (not.null))
  .(map BL.unpack)
  .(BL.splitWith (\c ->
                    c == ' '
                   || c == ','))
  .(BL.pack)

init_args :: Args
init_args = Args [] "." []
