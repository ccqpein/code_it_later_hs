import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Lazy              as Map
import           Text.Regex.TDFA

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
    Content_with_keyword _ -> crumb
    Content co              ->
      let (_,_,temp,_) = co =~ x :: (String,String,String,[String]) in
        if temp == ""
        then keyword_filter xs crumb
        else Content_with_keyword (x, temp)


-- pick crumbs out of file
pickout_from_line :: [Keyword_regex] -> [Comment_regex] -> String -> Crumb
pickout_from_line ks crs s = keyword_filter ks $ pick_comment_out crs s


read_comment_mark_map :: FilePath -> IO (Maybe Object)
read_comment_mark_map f = do
  json <- readFile f
  return $ decode $ BL.pack json

-- get_key_out_of_map :: Object -> [String]
-- result <- read_comment_mark_map "./comments.json"


-- first argument is file path
-- pickout_from_file :: FilePath -> [Keyword_regex] -> [Comment_regex] -> [Line_Crumb]


