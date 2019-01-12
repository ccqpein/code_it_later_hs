module Main where

import           Args

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict        as Map
import qualified Data.Text                  as T
import qualified Data.Vector
import           Text.Regex.TDFA

import           Distribution.Simple.Utils  (getDirectoryContentsRecursive)
import           GHC.IO.Handle
import           GHC.IO.IOMode
import           System.IO

type Comment_regex = String
type Keyword_regex = String

data Crumb =  None
  | Content String
  | Content_with_keyword (String, String)
  deriving (Show,Eq)

data Line_Crumb = Line_Crumb {
  linenum :: Int,
  cmb     :: Crumb
  }
  deriving (Show,Eq)


-- make regexs
make_comment_regex :: String -> Comment_regex
make_comment_regex sym = ".*" ++ sym ++ "+:= *"

make_keyword_regex :: String ->  Keyword_regex
make_keyword_regex keyword = keyword ++ ": *"


-- pick comment out line by line
pick_comment_out :: [Comment_regex] -> String -> Crumb
pick_comment_out [] _     =    None
pick_comment_out (sym:syms) line =
  let (_, _, temp, _) = line =~ sym ::(String,String,String,[String]) in
    if temp == ""
    then pick_comment_out syms line
    else Content temp


-- filter keywords out
keyword_filter :: [Keyword_regex] -> Crumb -> Crumb
keyword_filter [] a = a
keyword_filter (x:xs) crumb =
  case crumb of
    Content co              ->
      let (_,_,temp,_) = co =~ x :: (String,String,String,[String]) in
        if temp == ""
        then keyword_filter xs crumb
        else Content_with_keyword ((init x), temp)
    _ -> crumb


-- pick crumbs out of file
pickout_from_line :: [Keyword_regex] -> [Comment_regex] -> String -> Crumb
pickout_from_line ks crs s = keyword_filter ks $ pick_comment_out crs s


-- from json file
read_comment_mark_map_file :: FilePath -> IO (Maybe Object)
read_comment_mark_map_file f = do
  json <- readFile f
  return $ decode $ BL.pack json


-- from string directly
read_comment_mark_map :: String -> IO (Maybe Object)
read_comment_mark_map s = do return $ decode $ BL.pack s


get_comment_out_of_map :: String -> Maybe Object -> [String]
get_comment_out_of_map _ Nothing = []
get_comment_out_of_map k (Just obj) =
  case Map.lookup (T.pack k) obj of
    Just v ->
      case v of
        String s -> [T.unpack s]
        Array a  -> Data.Vector.toList $ Data.Vector.map (\x -> case x of
                                                                  String s -> T.unpack s
                                                                  _ -> "") a
    Nothing -> []


get_keys_out_of_map :: Maybe Object -> [String]
get_keys_out_of_map Nothing    = []
get_keys_out_of_map (Just obj) = map T.unpack (Map.keys obj)


inner_parser :: Handle -> (String -> Crumb) -> Int -> [Line_Crumb] -> IO [Line_Crumb]
inner_parser inh func ln re = do
  ineof <- hIsEOF inh
  if ineof
    then return re
    else do inpStr <- hGetLine inh
            let this_line_crumb = func inpStr
            case this_line_crumb of
              None -> inner_parser inh func (1 + ln) re
              _ -> inner_parser inh func (1 + ln)
                (Line_Crumb{ linenum = ln,cmb = (func inpStr)} : re)


pickout_from_file :: [Keyword_regex] -> [Comment_regex] -> FilePath -> IO [Line_Crumb]
pickout_from_file kr cr  path = do
  inh <- (openFile path ReadMode)
  inner_parser inh (pickout_from_line kr cr) 0 []


if_have_filetype :: [String] -> [FilePath] -> [FilePath]
if_have_filetype types = filter (filter_filetype types)


filter_filetype :: [String] -> FilePath -> Bool
filter_filetype _ f
  | (length (BL.split '.' (BL.pack f))) == 1 = False
filter_filetype ss f =
  let fp = BL.pack f in
    iter_filter_ft ss fp
  where
    iter_filter_ft [] _ = False
    iter_filter_ft (x:xs) fpp
      | (last $ BL.split '.' fpp) == (BL.pack x)  =  True
      | otherwise = iter_filter_ft xs fpp


iter_all_files :: [FilePath] -> (FilePath -> IO [Line_Crumb]) -> [IO (FilePath, [Line_Crumb])]
iter_all_files files func = map (\f -> do
                                    cmbs <- func f
                                    return $ (f, cmbs)
                                ) files


-- for test, local go file
main :: IO ()
main = do
  json <- read_comment_mark_map_file "./comments.json"

  let filetypes = get_keys_out_of_map json
  -- if use several filetypes, this design may cause some bugs. need to fix it
  files <- fmap (if_have_filetype filetypes) (getDirectoryContentsRecursive ".")

  let comment_keys = map make_comment_regex (get_comment_out_of_map "go" json)
  let func = pickout_from_file [] comment_keys
  mconcat $ map (\e -> do
                    (fp, crumbs) <- e
                    print fp
                    print crumbs)
    (iter_all_files files func)

