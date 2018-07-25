module GUBS.Utils where


import qualified Control.Monad.Trace          as MTR
import qualified Debug.Trace                  as TR
import qualified System.IO                    as IO
import qualified Text.PrettyPrint.ANSI.Leijen as PP


class PrettySexp a where
  prettySexp :: a -> PP.Doc

ppSexp :: [PP.Doc] -> PP.Doc
ppSexp = PP.encloseSep PP.lparen PP.rparen PP.space

ppCall :: PP.Pretty e => String -> [e] -> PP.Doc
ppCall n args = ppSexp (PP.text n : map PP.pretty args)

ppBin :: (PP.Pretty a, PP.Pretty b) => (PP.Doc -> PP.Doc) -> String -> a -> b -> PP.Doc
ppBin par f a b = par (PP.pretty a PP.</> PP.text f PP.<+> PP.pretty b)

renderPretty :: PP.Pretty e => e -> String
renderPretty d = PP.displayS (PP.renderSmart 1.0 200 (PP.pretty d)) ""

hPutDocLn :: PP.Pretty e => IO.Handle -> e -> IO ()
hPutDocLn h = IO.hPutStrLn h . renderPretty

putDocLn :: PP.Pretty e => e -> IO ()
putDocLn = hPutDocLn IO.stdout

putDocErrLn :: PP.Pretty e => e -> IO ()
putDocErrLn = hPutDocLn IO.stderr


tracePretty :: PP.Pretty e => String -> e -> e
tracePretty s d = TR.trace (renderPretty (PP.text s PP.<+> PP.pretty d)) d


logMsg :: MTR.MonadTrace String m => PP.Pretty e => e -> m ()
logMsg = MTR.trace . renderPretty

logBlk :: MTR.MonadTrace String m => PP.Pretty e => e -> m a -> m a
logBlk = MTR.scopeTrace . renderPretty

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) m = m a
whenJust _        _ = pure ()

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM g t e = do {b <- g; if b then t else e}
