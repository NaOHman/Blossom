
-- Validate ArgDecs in EFix and Lambdas
-- Add Classes, Data Decs, and Given Statements
-- Transform Returns from case statements into case branches
-- Incomplete Case statements are fallible


posAfterKw = "You can't define positional arugment that come after keyword arguments"
posAfterS = "You can't define positional argument after you've defined an argument that aggregates a list"
multiplePS = "You can't define multiple argument that aggregate lists"
multipleKS = "You can't define multiple arguments that aggregate keyword arugments"
{-addK a = do-}
    {-n <- lName-}
    {-v <- equals' *> cTimeLit-}
    {-c <- opCons-}
    {-return  $ a {keyword = (n,v,c):keyword a}-}

{-addP a = do -}
    {-n <- lName-}
    {-c <- opCons-}
    {-case arrArgs a of-}
        {-Nothing -> case keyword a of-}
             {-[] -> return $ a {positional = (n,c):positional a}-}
             {-_  -> fail posAfterKw-}
        {-Just _  -> fail posAfterS-}

{-addPS a = do-}
    {-n <- symbol "*" *> lName-}
    {-c <- opCons-}
    {-case arrArgs a of-}
        {-Nothing -> return $ a {arrArgs = Just (n,c)}-}
        {-Just _  -> fail multiplePS-}

{-addKS a = do-}
    {-n <- symbol "**" *> lName-}
    {-c <- opCons-}
    {-case kwArgs a of-}
        {-Nothing -> return $ a {kwArgs = Just (n, c)}-}
        {-Just _  -> fail multipleKS-}
