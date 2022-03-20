{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Edit where

import Data.Maybe
import Data.Bool (bool)
import Data.List
import qualified Data.Map as Map
import Distribution.Simple.Utils (xargs)
import System.IO
import Control.Monad

import Sum ( add_price, wordsWhen )
import Add (addPrice)
import Struct


deletePrice ::  String -> [String] ->Maybe [String]
deletePrice b a 
    | head a == b =  Nothing
    | otherwise = Just a

deleteIng ::  String -> [String] ->Maybe [String]
deleteIng b a 
    | head a == b =  Nothing
    | otherwise = Just a

deleteRcept :: String -> String -> Maybe String
deleteRcept b a 
    | a == b =  Nothing
    | otherwise = Just a

toString_:: [String] -> String
toString_ [a,b] = do
    let
        ing = a
        price_ = b
        stings = ing ++ " " ++ price_  ++ "\n"
    stings

toStringRec :: String -> String
toStringRec a = do
    let
        ing = a
        stings = ing ++ " "
    stings

toStringing:: [String] -> String
toStringing [a,b,c] = do
    let
        stings = a ++ " " ++ b ++ " " ++ c ++"\n"
    stings

editPrice :: IO()
editPrice = do
    
    recipe_handle <- openFile "price.txt" ReadMode
    price_contents <- hGetContents recipe_handle
    let
      list_price = wordsWhen (=='\n') price_contents
      all_price = map (wordsWhen (==' '))  list_price
    mapM_ print all_price
    print "Write  ingredient and price to edit ingredient example: tomato 1"
    new_price <- getLine
    let
      price_list = wordsWhen (==' ') new_price
      delete_list = map (deletePrice (head price_list)) all_price 
      clear_list = catMaybes delete_list
      list_one = map toString_ clear_list
      list_to_sting = intercalate "" list_one
      result_string = list_to_sting ++ head price_list ++ " " ++ last price_list ++ "\n"
    when (length result_string > 0) $
       writeFile "price.txt" result_string

editIng :: IO()
editIng = do
    
    recipe_handle <- openFile "new_ing.txt" ReadMode
    price_contents <- hGetContents recipe_handle
    let
      list_price = wordsWhen (=='\n') price_contents
      -- разбиваем по пробелам
      all_price = map (wordsWhen (==' '))  list_price
    
    mapM_ print all_price
    print "Write  ingredient, recept and grams to edit  example: pecans Millionaire_Pie 94"
    new_price <- getLine
    let
      price_list = wordsWhen (==' ') new_price
      delete_list = map (deleteIng (head price_list) ) all_price
      clear_list = catMaybes delete_list 
      list_one = map toStringing clear_list
      list_to_sting = intercalate "" list_one
      result_string = list_to_sting ++ head price_list ++ " " ++ price_list !! 1 ++ " " ++ last price_list ++ "\n"
    when (length result_string > 0) $
        writeFile "test.txt" result_string

editWork :: IO()
editWork = do
   
    recipe_handle <- openFile "work_price.txt" ReadMode
    price_contents <- hGetContents recipe_handle
    let
      
      list_price = wordsWhen (=='\n') price_contents
      all_price = map (wordsWhen (==' '))  list_price
    mapM_ print all_price
    print "Write  recept and price to edit example: School_Girl_Pickles 100"
    new_price <- getLine
    let 
      price_list = wordsWhen (==' ') new_price
      delete_list = map (deletePrice (head price_list)) all_price 
      clear_list = catMaybes delete_list
      list_one = map toString_ clear_list
      list_to_sting = intercalate "" list_one
      result_string = list_to_sting ++ head price_list ++ " " ++ last price_list ++ "\n"
    when (length result_string > 0) $
       writeFile "work_price.txt" result_string


editRepect :: IO()
editRepect = do
    recipe_handle <- openFile "new_recept.txt" ReadMode
    recept_contents <- hGetContents recipe_handle
    let
      list_price = wordsWhen (==' ') recept_contents
    mapM_ print list_price
    print "Write the old recipe, and then the new one  example: Creamy_Corn Creamy_Corns"
    new_recept <- getLine
    let
      recept = wordsWhen (==' ') new_recept
      delete_list = map (deleteRcept (head recept)) list_price 
      clear_list = catMaybes delete_list
      list_one = map toStringRec clear_list
      list_to_sting = intercalate "" list_one
      result_string = list_to_sting ++ last recept ++ " "

    when (length result_string > 0) $
       writeFile "new_recept.txt" result_string