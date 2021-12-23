{-# LANGUAGE StrictData #-}
module Language.Cimple.IO
    ( parseFile
    , parseFiles
    , parseProgram
    , parseText
    ) where

import           Control.Monad                   ((>=>))
import           Control.Monad.State.Lazy        (State, evalState, get, put)
import qualified Data.ByteString                 as BS
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import           Language.Cimple.AST             (Node (..))
import           Language.Cimple.Lexer           (Lexeme, runAlex)
import qualified Language.Cimple.Parser          as Parser
import           Language.Cimple.Program         (Program)
import qualified Language.Cimple.Program         as Program
import           Language.Cimple.TranslationUnit (TranslationUnit)
import qualified Language.Cimple.TreeParser      as TreeParser

type CacheState a = State (Map String Text) a

cacheText :: String -> CacheState Text
cacheText s = do
    m <- get
    case Map.lookup s m of
        Nothing -> do
            let text = Text.pack s
            put $ Map.insert s text m
            return text
        Just text ->
            return text


process :: [Node a (Lexeme String)] -> [Node a (Lexeme Text)]
process stringAst =
    evalState (mapM (mapM (mapM cacheText)) stringAst) Map.empty


parseText :: Text -> Either String [Node () (Lexeme Text)]
parseText contents =
    process <$> res
  where
    res :: Either String [Node () (Lexeme String)]
    res =
        runAlex (Text.unpack contents) Parser.parseTranslationUnit

parseTextStrict :: Text -> Either String [Node () (Lexeme Text)]
parseTextStrict = parseText >=> TreeParser.toEither . TreeParser.parseTranslationUnit


parseFile :: FilePath -> IO (Either String (TranslationUnit Text))
parseFile source =
    addSource . parseTextStrict . Text.decodeUtf8 <$> BS.readFile source
  where
    -- Add source filename to the error message, if any.
    addSource (Left err) = Left $ source <> ":" <> err
    -- If there's no error message, record the source filename in the returned
    -- TranslationUnit.
    addSource (Right ok) = Right (source, ok)


parseFiles' :: [FilePath] -> IO (Either String [TranslationUnit Text])
parseFiles' sources = sequenceA <$> traverse parseFile sources


parseFiles :: [FilePath] -> IO (Either String [TranslationUnit Text])
parseFiles sources = fmap Program.toList . (>>= Program.fromList) <$> parseFiles' sources


parseProgram :: [FilePath] -> IO (Either String (Program Text))
parseProgram sources = (>>= Program.fromList) <$> parseFiles' sources
