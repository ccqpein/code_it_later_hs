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
import           System.Environment
import           System.IO


type FileType = String
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
-- if [Keyword_regex] is empty it does nothing
pickout_from_line :: [Keyword_regex] -> [Comment_regex] -> String -> Crumb
pickout_from_line ks crs s =
  let temp = keyword_filter ks $ pick_comment_out crs s in
  case temp of
     None                     -> None
     Content_with_keyword (_) -> temp
     _                        -> if null ks then temp else None


-- from json file
read_comment_mark_map_file :: FilePath -> IO (Maybe Object)
read_comment_mark_map_file f = do
  jn <- readFile f
  return $ decode $ BL.pack jn


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
        _ -> []
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



pickout_filetype :: FilePath -> FileType
pickout_filetype f
  | f == "" = ""
  | otherwise = let temp = (BL.split '.' (BL.pack f)) in
  if (length temp) == 1 then "" else (BL.unpack $ last temp)


pickout_from_file_with_filetype :: Map.HashMap FileType ([Keyword_regex],[Comment_regex]) -> FilePath -> IO [Line_Crumb]
pickout_from_file_with_filetype m f
  | null m = return []
  | otherwise = let thistype = pickout_filetype f in
                  if thistype == ""
                  then return []
                  else
                    let val = Map.lookup thistype m in
                      case val of
                        Just (krgs, crgs) -> pickout_from_file krgs crgs f
                        Nothing           -> return []


-- if_have_filetype :: [FileType] -> [FilePath] -> [FilePath]
-- if_have_filetype types = filter (check_filetype types)


check_filetype :: [FileType] -> FilePath -> Bool
check_filetype _ f
  | (length (BL.split '.' (BL.pack f))) == 1 = False
check_filetype ss f =
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


argvs_handle :: Args -> Maybe Object -> Map.HashMap FileType ([Keyword_regex],[Comment_regex])
argvs_handle a jn
  | a == init_args = -- no input, all filetype should be read
    let keyword_re = map make_keyword_regex (keywords a) in
      Map.fromList $ map (\x ->
                            (x, (keyword_re, map make_comment_regex (get_comment_out_of_map x jn))))
      (get_keys_out_of_map jn)
  | otherwise =
    let keyword_re = map make_keyword_regex (keywords a) in
      Map.fromList $ map (\x ->
                            (x, (keyword_re, map make_comment_regex (get_comment_out_of_map x jn))))
      (if null (filetypes a) then (get_keys_out_of_map jn) else (filetypes a))


format_print :: [IO (FilePath, [Line_Crumb])] -> IO ()
format_print [] = return ()
format_print (x:xs) = do
  (f, lc) <- x
  if null lc
    then format_print xs
    else do
    print (f,lc)
    format_print xs


default_table :: String
default_table = "{\"clj\" : \";\", \
\\"go\" : [\"//\",\"/\\\\*\"],\
\\"py\" : \"#\",\
\\"lisp\":\";\",\
\\"hs\":\"-- \",\
\\"rs\":[\"//\",\"/\\\\*\"],\
\\"el\":\";\"}"

{-

What I need next:
- documents && readme
- clean some function
- more features
  + format print <-****************** important
  + args can be several
  + can input more json map
- more test

-}

-- for test, local go file
main :: IO ()
main = do
  args <- getArgs
  let args_data = parse_args args init_args

  -- print args
  -- json <- read_comment_mark_map_file "./comments.json"
  json_data <- read_comment_mark_map default_table


  -- let filetypes = get_keys_out_of_map json_data
  -- if use several filetypes, this design may cause some bugs. need to fix it
  -- files <- fmap (if_have_filetype filetypes) (getDirectoryContentsRecursive ".")
  files <- getDirectoryContentsRecursive "."

  -- let comment_keys = map make_comment_regex (get_comment_out_of_map "go" json)
  -- let comment_keys_py = map make_comment_regex (get_comment_out_of_map "py" json)
  -- let table = Map.fromList [("go",([],comment_keys)), ("py", ([],comment_keys_py))]
  -- print args_data
  let table = argvs_handle args_data json_data
  -- print table
  let func = pickout_from_file_with_filetype table
  -- mconcat $
  format_print (iter_all_files files func)

