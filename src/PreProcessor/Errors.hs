module PreProcessor.Errors where

posAfterKw p = err p "You can't define positional arugment that come after keyword arguments"
posAfterS p = err p "You can't define positional argument after you've defined an argument that aggregates a list"
kwAfterSplat p = err p "You can't declare a keyword argument after an argument that aggregates keyword arguments"
multiplePS p = err p "You can't define multiple argument that aggregate lists"
multipleKS p = err p "You can't define multiple arguments that aggregate keyword arugments"
duplicateFunc p n= err p ("Duplicate function name " ++ n)
duplicateArg p n = err p ("Two args with the name" ++ n)

errDataDeclared n pos = err pos $ "Data Name " ++ n ++ " is already defined at"
tooFewArgs pos n = err pos $ "Too few arguments in call to fucntion " ++ n
tooManyArgs pos n = err pos $ "Too many arguments in call to fucntion " ++ n

badKWArg i n pos = err pos $ "Function " ++ n ++ " does not accept an arguments called" ++ i
parserError pos = err pos "Unexpected input, this is probably the fault of the parser"

splatImpossible p n = err p $ "Function " ++ n ++ " has already taken to many arguments and splatting will cause an error"
ambiguousConstructor pos n = err pos ("Ambiguous Constructor name " ++ n)

unknownConstructor pos n = err pos ("Unknown Constructor " ++ n)
unknownFunction pos n = err pos ("Unknown Function " ++ n)

warnMultConstrs p n = warn p $ "Multiple constructors using the name " ++ n

errDataName p = err p "Invalid data name"

warn p s = return $ "Warning at " ++ show p ++ ": " ++ s

err p s = fail $ "Error at " ++ show p ++ ": " ++ s

catWarnings ("":ws) = catWarnings ws
catWarnings (w:ws) = w : catWarnings ws
catWarnings _ = []
