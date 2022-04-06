module Main where
import           System.Console.Haskeline
import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)
import HW3.Base
import Data.Ratio ((%))
import HW3.Action (HIO (..), HiPermission (..))
import qualified Data.Set as Set

main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return()
        Just ":q" -> return ()
        Just "" -> loop
        Just input -> do
          let parsed = parse input
          outputStrLn $ case parsed of
            Left parseError -> show parseError
            Right parseValue -> do
              v <- eval parseValue
              case v of
--              (eval parseValue) >>= (\x -> case x of
                Left evalError  -> show evalError
                Right evalValue -> show (prettyValue evalValue)
--                )
          loop