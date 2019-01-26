module Backup where

import           GHC.Base (liftA2)
import           Main

-----------------------------
-- this function is not as effection as getDirectoryContentsRecursive
-- abandoned
get_all_files :: FilePath -> IO [FilePath]
get_all_files f = do
  fs <- listDirectory f
  get_all_files_recur [] (map ((f ++ "/") ++) fs)
  where
    get_all_files_recur re (x:xs) = do
      status <- getFileStatus x
      if isDirectory status
        then do
          fs <- listDirectory x
          get_all_files_recur re ((map ((x ++ "/") ++) fs) ++ xs)
        else do
          get_all_files_recur (re ++ [x]) xs
    get_all_files_recur re [] = return re

---------------------------
---------------------------

check_filetype :: [FileType] -> FilePath -> Bool
check_filetype _ f
  | takeExtension f == "" = False
check_filetype ss f =
  let fp = takeExtension f
   in iter_filter_ft ss fp
  where
    iter_filter_ft [] _ = False
    iter_filter_ft (x:xs) fpp
      | fpp == x = True
      | otherwise = iter_filter_ft xs fpp


---------------------------------------------
-- this is test function try to don't use map
-- useless, nothing change about memory cost
iter_all_files2 ::
     [FilePath]
  -> (FilePath -> IO [Line_Crumb])
  -> IO [(FilePath, [Line_Crumb])]
iter_all_files2 [] _ = return []
iter_all_files2 (f:xs) func =
  let temp = do
        cmbs <- func f
        return $ (f, cmbs) in
    liftA2 (:) temp (iter_all_files2 xs func)
------------------------------------------------


format_print_out2 :: [(FilePath, [Line_Crumb])] -> IO ()
format_print_out2 [] = return ()
format_print_out2 (x:xs) = let (f,lc ) = x in
  case lc of
    [] -> format_print_out2 xs
    (_:_) -> do
      printf "|-- %s" f
      printf "%s\n\n" (format_print lc)
      format_print_out2 xs


main :: IO ()
main = do
  args <- getArgs
  let args_data = parse_args args init_args
  json_data <- read_comment_mark_map default_table
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

  -- code below is use native forkIO function to turn on concurrency
  -- but I do not want to write code for blocking main thread until all...
  -- .. other threads end. So I use 3rd party package to do this
  --forkIO (format_print_out (iter_all_files b2 func))
  --forkIO (format_print_out (iter_all_files a2 func))
  --forkIO (format_print_out (iter_all_files b1 func))
  --format_print_out (iter_all_files a1 func)

  --(_,wait1) <- Thread.forkIO $ format_print_out (iter_all_files a1 func)
  --(_,wait2) <- Thread.forkIO $ format_print_out (iter_all_files a2 func)
  --(_,wait3) <- Thread.forkIO $ format_print_out (iter_all_files b1 func)
  --(_,wait4) <- Thread.forkIO $ format_print_out (iter_all_files b2 func)
  --_ <- wait1
  --_ <- wait2
  --_ <- wait3
  --_ <- wait4

  a1T <- (iter_all_files2 a1 func)
  a2T <- (iter_all_files2 a2 func)
  b1T <- (iter_all_files2 b1 func)
  b2T <- (iter_all_files2 b2 func)
  format_print_out2 $ (force a1T `par` (force a2T `par` (force b1T `par` (force b2T `par` (a1T ++ a2T ++ b1T ++ b2T)))))

  return ()
