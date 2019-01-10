-- := TODO: I need format

type Comment_mark = String

data Crumb =  None
  | Content String
  | Content_with_keyword (String, String)
  deriving (Show)

pick_comment_out :: [Comment_mark] -> String -> Crumb
pick_comment_out [] _     =    None
-- := TODO: need regex package
pick_comment_out (x:xs) y = Content_with_keyword (x,y)

