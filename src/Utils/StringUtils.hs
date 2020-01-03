module Utils.StringUtils where

safeShowList :: Show a => [a] -> String
safeShowList l = case l of
    _:_ -> foldr1 (\el -> ((el ++ ", ") ++)) (fmap show l)
    _ -> ""

indent :: String -> String
indent s = "    " ++ s
