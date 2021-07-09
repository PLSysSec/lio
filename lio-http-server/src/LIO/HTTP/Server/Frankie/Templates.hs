{-# LANGUAGE GADTs, DataKinds, QuasiQuotes, TemplateHaskell #-}
module LIO.HTTP.Server.Frankie.Templates where

import Text.Mustache
import Data.ByteString.Lazy as LBS
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

applyTemplate :: ToMustache m => m -> Template -> LBS.ByteString
applyTemplate = fmap (LBS.fromStrict . encodeUtf8) <$> flip substitute

-- a quasiquoter that parses into a Mustache `Template`.
mustache :: QuasiQuoter
mustache = QuasiQuoter{quoteExp = myTemplateParser
                      ,quotePat = qqerr, quoteType=qqerr, quoteDec= qqerr}
  where
    qqerr = error "wrong quasiquoter type"
    myTemplateParser s = case compileTemplate "" (T.pack s) of
      Left err -> fail $ show err
      Right template -> sigE [| template |] [t| Template |]

--import Text.Regex

-- https://mustache.github.io/mustache.5.html
{-
data Phase = Lex | Parsed

data Chunk (a :: Phase) where
  RawChunk :: String -> Chunk a
  VarChunk :: String -> Chunk a
  SectionBegin :: String -> Chunk 'Lex
  SectionEnd :: String -> Chunk 'Lex
  Section :: [Chunk 'Parsed] -> Chunk 'Parsed

newtype Template = Template [Chunk 'Parsed]

lexTemplate :: String -> [Chunk 'Lex]
lexTemplate s = go (take 2 s == "{{") splits
  where
    splits = splitRegex regex s
    regex = mkRegex "\\{\\{|\\}\\}"
    -- TODO flesh this out
    go _ [] = []
    go True (x:xs) = VarChunk x : go False xs
    go False (x:xs) = RawChunk x : go True xs

coaleseChunks :: [Chunk 'Lex] -> Template
coaleseChunks = Template . go []
  where
    -- done
    go _ [] = []
    -- close section, top of stack
    go [(SectionBegin b, body)] (SectionEnd e :xs)
      | e == b = Section (reverse body) : go xs
      | otherwise = error "unclosed section"
    -- close section, nested
    go ((SectionBegin b, body) : stk) (SectionEnd e :xs)
      | e == b = go (Section (reverse body) : stk) xs
      | otherwise = error "unclosed section"

    -- open section
    go stk (SectionBegin b : xs)
      = go ((SectionBegin b, []) : stk) xs

    -- normal token, nested
    go ((SectionBegin b, body) : stk) (x : xs)
      = go ((SectionBegin b, x : body):stk) xs

    -- normal token, top of stack
    go [] (x:xs) = x : go [] xs


parseTemplate :: String -> Template
parseTemplate = coaleseChunks . lexTemplate
-}

-- class Templatable a where
--   insert :: a -> String
-- instance Templatable String where
--   insert = id
-- instance Show a => Templatable a where
--   insert = show
