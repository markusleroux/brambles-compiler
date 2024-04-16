module Brambles.Frontend where

import Protolude hiding (TypeError)
import qualified Prelude (Show(..))

import Control.Monad.Except (liftEither, modifyError)

import qualified Brambles.Frontend.AST as AST
import Brambles.Frontend.Parser as P (programP)
import Brambles.Frontend.Symbolize as S
import qualified Brambles.Frontend.Typecheck as TC

import Text.Megaparsec (parse, ParseErrorBundle, errorBundlePretty)

data FrontendError
  = ParseError (ParseErrorBundle Text Void)  -- TODO: wrap megaparsec errors
  | SymbolError S.SymbolizeException
  | TypeError TC.TypeError

instance Show FrontendError where
  show (ParseError e) = toS $ errorBundlePretty e
  show (SymbolError e) = show e
  show (TypeError e) = show e

-- TODO: make this generic over a stream
generateAST :: Monad m => Text -> Text -> ExceptT FrontendError m (AST.Module Int 'AST.Typed)
generateAST file = doParse >=> doRename >=> doTypecheck
  where
    doParse     = asExceptT ParseError  . parse P.programP (toS file)
    doRename    = asExceptT SymbolError . S.runIncrementalSymbolize . S.renameModule
    doTypecheck = asExceptT TypeError   . TC.runTypechecking . TC.inferModule

asExceptT :: MonadError FrontendError m => (a -> FrontendError) -> Either a b -> m b
asExceptT c = modifyError c . liftEither

