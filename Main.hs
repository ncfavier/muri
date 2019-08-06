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
                _ -> die "No proof."
            _ -> die "No parse."
        _ -> usage
    where usage = die "usage: muri TYPE"
