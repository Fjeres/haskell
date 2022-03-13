{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}


module Add where

import Data.Maybe
import Data.Bool (bool)
import Data.List
import qualified Data.Map as Map
import Distribution.Simple.Utils (xargs)
import System.IO
import Control.Monad
import Sum
--соединить
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys


addRecept :: IO()
addRecept = do
    recipe_handle <- openFile "recipe.txt" ReadMode
    recipe_contents <- hGetContents recipe_handle
    print "Write new recept"
    new_recept <- getLine 
    let 
       temp_recept = recipe_contents ++ " " ++ new_recept
    when (length temp_recept > 0) $
       writeFile "recipe.txt" temp_recept


addIngredient :: IO ()
addIngredient = do
    -- чтение ингредиенты
    ingredient_handle <- openFile "ingredient.txt" ReadMode
    ingredient_contents <- hGetContents ingredient_handle

    print "Write new ingredient"
    print "Write ingredient, recept and grams example: tomato pizza 100"
    new_ingredient <- getLine 
    let 
       temp_ingredient = ingredient_contents ++ "\n" ++ new_ingredient
    when (length temp_ingredient > 0) $
       writeFile "ingredient.txt" temp_ingredient

addPrice :: IO ()
addPrice = do

    print "Write new price"
    print "Write price and price for 1 gram example: tomato 1"
    price_handle <- openFile "price.txt" ReadMode
    price_contents <- hGetContents price_handle
    new_price <- getLine
    let 
       temp_price = price_contents ++ "\n" ++ new_price
    when (length temp_price > 0) $
       writeFile "price.txt" temp_price

addWork :: IO ()
addWork = do

    print "Write new price work"
    print "Write recept and price: pizza 150"
    work_handle <- openFile "price_work.txt" ReadMode
    work_contents <- hGetContents work_handle
    new_price_work <- getLine
    let 
       temp_work = work_contents ++ "\n" ++ new_price_work
    when (length temp_work > 0) $
       writeFile "price_work.txt" temp_work