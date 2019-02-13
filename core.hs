module Main where

-- import arg.hs
import           Args

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict        as Map
import qualified Data.Text                  as T
import qualified Data.Vector

import           Distribution.Simple.Utils  (getDirectoryContentsRecursive)
import           GHC.IO.Handle
import           GHC.IO.IOMode
import           System.Environment
import           System.FilePath.Posix      (takeExtension)
import           System.IO
import           Text.Printf                (printf)
import           Text.Regex                 as TR

import qualified Control.Concurrent.Thread  as Thread (forkIO)
import           Control.DeepSeq            (force)


-- define several types
type FileType = String

type Comment_regex = String

type Keyword_regex = String

class Format_print a where
  format_print :: a -> String


-- define my dear crumb(s), and instance it
data Crumb
  = None
  | Content !String
  | Content_with_keyword !(String, String)
  deriving (Show, Eq)

instance Format_print Crumb where
  format_print None                          = ""
  format_print (Content s)                   = s
  format_print (Content_with_keyword (k, s)) = k ++ s


data Line_Crumb = Line_Crumb
  { linenum :: !Int
  , cmb     :: !Crumb
  } deriving (Show, Eq)

instance Format_print Line_Crumb where
  format_print Line_Crumb {linenum = l, cmb = cmb_} =
    "  |-- Line " ++ show l ++ ": " ++ format_print cmb_

instance (Format_print c) => Format_print [c] where
  format_print (x:xs) = format_print xs ++ "\n" ++ format_print x
  format_print []     = ""


-- make reges
make_comment_regex :: String -> Comment_regex
make_comment_regex sym = sym ++ "+:= *"

make_keyword_regex :: String -> Keyword_regex
make_keyword_regex keyword = keyword ++ ": *"


-- pick comment out line by line
pick_comment_out :: [TR.Regex] -> String -> Crumb
pick_comment_out [] _ = None
pick_comment_out (sym:syms) line =
  let temp = TR.splitRegex sym line
   in if null temp || length temp == 1
        then pick_comment_out syms line
        else Content (last temp)

-- filter keywords out
-- first argv is (keyword, keyword_regex_pattern)
keyword_filter :: [(Keyword_regex, TR.Regex)] -> Crumb -> Crumb
keyword_filter [] a = a
keyword_filter (x:xs) crumb =
  case crumb of
    Content co ->
      let temp = TR.splitRegex (snd x) co
       in if null temp || length temp == 1
            then keyword_filter xs crumb
            else Content_with_keyword ((init $ fst x), (last temp))
    _ -> crumb

-- pick crumbs out of file
-- if [Keyword_regex] is empty it does nothing
pickout_from_line ::
     [(Keyword_regex, TR.Regex)] -> [TR.Regex] -> String -> Crumb
pickout_from_line ks crs s =
  let temp = keyword_filter ks $ pick_comment_out crs s
   in case temp of
        None -> None
        Content_with_keyword (_) -> temp
        _ ->
          if null ks
            then temp
            else None


-- from json file
read_comment_mark_map_file :: FilePath -> IO (Maybe Object)
read_comment_mark_map_file f = do
  jn <- readFile f
  return $ decode $ BL.pack jn


-- from string directly
read_comment_mark_map :: String -> IO (Maybe Object)
read_comment_mark_map s = do
  return $ decode $ BL.pack s


-- give filetype of souce code and return its comments mark(s)
get_comment_out_of_map :: String -> Maybe Object -> [String]
get_comment_out_of_map _ Nothing = []
get_comment_out_of_map k (Just obj) =
  case Map.lookup (T.pack k) obj of
    Just v ->
      case v of
        String s -> [T.unpack s]
        Array a ->
          Data.Vector.toList $
          Data.Vector.map
            (\x ->
               case x of
                 String s -> T.unpack s
                 _        -> "")
            a
        _ -> []
    Nothing -> []


-- pick all filetypes supported out
get_keys_out_of_map :: Maybe Object -> [String]
get_keys_out_of_map Nothing    = []
get_keys_out_of_map (Just obj) = map T.unpack (Map.keys obj)


-- this function used by pickout_from_file
-- input file handle, single line -> crumb function, line number start(for recursive)
-- and [line_Crumb] (for recursive)
inner_parser ::
     Handle -> (String -> Crumb) -> Int -> [Line_Crumb] -> IO [Line_Crumb]
inner_parser inh func ln re = do
  ineof <- hIsEOF inh
  if ineof
    then return re
    else do
      inpStr <- hGetLine inh
      let this_line_crumb = func inpStr
      case this_line_crumb of
        None -> inner_parser inh func (1 + ln) re
        _ ->
          inner_parser
            inh
            func
            (1 + ln)
            (Line_Crumb {linenum = ln, cmb = this_line_crumb} : re)

-- pick line_crumb out from file
pickout_from_file ::
     [Keyword_regex] -> [Comment_regex] -> FilePath -> IO [Line_Crumb]
pickout_from_file kr cr path = do
  inh <- (openFile path ReadMode)
  inner_parser
    inh
    (pickout_from_line (map (\x -> (x, TR.mkRegex x)) kr) (map TR.mkRegex cr))
    1
    []

-- pick line_crumb out from file
pickout_from_file_with_filetype ::
     Map.HashMap FileType ([Keyword_regex], [Comment_regex])
  -> FilePath
  -> IO [Line_Crumb]
pickout_from_file_with_filetype m f
  | null m = return []
  | otherwise =
    let thistype = takeExtension f
     in if thistype == ""
          then return []
          else let val = Map.lookup thistype m
                in case val of
                     Just (krgs, crgs) -> pickout_from_file krgs crgs f
                     Nothing           -> return []


-- merge filepath to [Line_Crumb]
iter_all_files ::
     [FilePath]
  -> (FilePath -> IO [Line_Crumb])
  -> [IO (FilePath, [Line_Crumb])]
iter_all_files files func =
  map
    (\f -> do
       cmbs <- func f
       return $ (f, cmbs))
    files

argvs_handle ::
     Args
  -> Maybe Object
  -> Map.HashMap FileType ([Keyword_regex], [Comment_regex])
argvs_handle a jn
  | a == init_args -- no input, all filetype should be read
   =
    let keyword_re = map make_keyword_regex (keywords a)
     in Map.fromList $
        map
          (\x ->
             ( x
             , ( keyword_re
               , map make_comment_regex (get_comment_out_of_map x jn))))
          (get_keys_out_of_map jn)
  | otherwise =
    let keyword_re = map make_keyword_regex (keywords a)
     in Map.fromList $
        map
          (\x ->
             ( x
             , ( keyword_re
               , map make_comment_regex (get_comment_out_of_map x jn))))
          (if null (filetypes a)
             then (get_keys_out_of_map jn)
             else (filetypes a))

format_print_out :: [IO (FilePath, [Line_Crumb])] -> IO ()
format_print_out [] = return ()
format_print_out (x:xs) = do
  (f, lc) <- x
  case lc of
    [] -> format_print_out xs
    (_:_) -> do
      printf "|-- %s" f
      printf "%s\n\n" (format_print lc)
      format_print_out xs


-- this is init json string which embed code, so binary file will
-- support this lanuage automaticly
default_table :: String
default_table =
  "{\".clj\" : \";\", \
\\".go\" : [\"//\",\"/\\\\*\"],\
\\".py\" : \"#\",\
\\".lisp\":\";\",\
\\".hs\":\"-- \",\
\\".rs\":[\"//\",\"/\\\\*\"],\
\\".el\":\";\"}"

{-

What I need next:
- documents && readme
- clean some function
- more features
  + can input more json map
- more test
- use System.FilePath.Posix to handle filetype and path function (done)

-}

{--
try to optimize some functions by making it strict, failed.
there are where time/memory cost most:
- 1. pick_comment_out cost most of time, I may need optimize regex again.
- 2. (:) cost most of memory at begining of program, still have no idea whats going on
--}

main :: IO ()
main = do
  args <- getArgs
  let args_data = parse_args args init_args

  -- read comments table
  json_data <- if (jsonx args_data) == ""
    then read_comment_mark_map default_table
    else read_comment_mark_map_file (jsonx args_data)

  files <-
    fmap (map (((dir args_data) ++ "/") ++)) $
    getDirectoryContentsRecursive (dir args_data)
  let table = argvs_handle args_data json_data
  let func = pickout_from_file_with_filetype table

  -- cut file list
  let leng = length (force files)
  let (a, b) = splitAt (leng `div` 2) files
  let (a1, a2) = splitAt (leng `div` 4) a
  let (b1, b2) = splitAt (leng `div` 4) b

  (_,wait1) <- Thread.forkIO $ format_print_out (iter_all_files a1 func)
  (_,wait2) <- Thread.forkIO $ format_print_out (iter_all_files a2 func)
  (_,wait3) <- Thread.forkIO $ format_print_out (iter_all_files b1 func)
  (_,wait4) <- Thread.forkIO $ format_print_out (iter_all_files b2 func)
  _ <- wait1
  _ <- wait2
  _ <- wait3
  _ <- wait4

  return ()
  --format_print_out (iter_all_files files func)
  --putStrLn $ "number of cores: " ++ show numCapabilities
