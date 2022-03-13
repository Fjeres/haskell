import Data.Maybe
import Data.Bool (bool)
import Data.List
import qualified Data.Map as Map
import Distribution.Simple.Utils (xargs)
import System.IO
import Control.Monad
import Struct
import Sum
import Add

main :: IO()
main = do
    print"Choose"
    print"1 give sum recept"
    print"2 add Recept"
    print"3 add Ingredient"
    print"4 add Price"
    print"5 add Work"
    print"6 to exit"
    do_ <- getLine
    if do_ == "1" then sum_ >> main
    else if do_ == "2"  then do addRecept >> main
    else if do_ == "3"  then do addIngredient >> main
    else if do_ == "4"  then do addPrice >> main
    else when (do_ == "5") $ do addWork >> main

    