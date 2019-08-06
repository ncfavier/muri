import System.Environment
import System.Exit
import Text.Read

import Proof

main = do
    args <- getArgs
    case args of
        [a] -> case readMaybe a of
            Just g -> case prove g of
                Just t -> print t
                _ -> die "Impossible."
            _ -> die "Couldn't parse."
        _ -> do
            arg0 <- getProgName
            die $ "Usage: " ++ arg0 ++ " TYPE"
