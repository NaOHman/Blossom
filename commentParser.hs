import Text.ParserCombinators.Parsec

codeFile = endBy line myEOL
line = sepBy nonBreakSpace  $ many (name <|> continuation)
continuation = char '\\' skipMany nonBreakSpace (char '\n')

nonBreakSpace = '\t' <|> ' '
name = many (alphaNum <|> char '_')
