{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
module Sum where




import Data.Maybe
import Data.Bool (bool)
import Data.List
import qualified Data.Map as Map
import Distribution.Simple.Utils (xargs)
import System.IO
import Control.Monad
import Struct


--Для добавления  в начало
add :: a -> [a] -> [[a]]
add a b = [a:b]



--добавить рецепт
add_stuct :: String -> Recipe
add_stuct a  = Recipe a
--добавить ингредиент
add_ing :: [String] -> Ingredient
add_ing [a,b,c] = Ingredient a b c
--добавить  цену за ингредиент
add_price :: [String] -> Price
add_price [a,b] = Price a b
-- добавить работу
add_work :: [String] -> PriceW
add_work [a,b] = PriceW a b
 --поиск Ingredient


--пример как можно вывести все по 1
print_ :: Show a => a -> IO ()
print_ temp = print temp

--разделение по условию
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


--получение ингредиент из структуры
get_ingredient :: Ingredient -> String
get_ingredient ing = ingredient ing
--получение валуе из структуры
get_value_ingredient :: Ingredient -> String
get_value_ingredient ing = value ing
--получение цены
get_price_value :: Price -> String
get_price_value ing = price ing


find_ :: String -> Ingredient -> Maybe Ingredient
find_ recept ing
    | recept == _recipe_ = Just ing
    | otherwise = Nothing
    where
        _recipe_ = recipe_ ing

find_work :: String -> PriceW -> Maybe PriceW
find_work recept work
    | recept == _recipe_ = Just work
    | otherwise = Nothing
    where
        _recipe_ = recipeW work


get_price :: [Price] -> String -> Maybe Price
get_price [] _ = Nothing
get_price (x:xs) findItem
    | name == findItem = Just x
    | otherwise = get_price xs findItem
    where
        name = ingredient_ x


calc_result :: Integer -> PriceW -> Integer
calc_result sum_value price_struct = res
    where
        num1 = read (priceW price_struct) :: Integer
        res = sum_value + num1


get_ing :: Price -> String
get_ing a = ingredient_ a

cacl_value :: [String] -> [String] -> Integer
cacl_value [] [] = 0
cacl_value (x:xs) (y:ys) = res
    where
        num1 = read y :: Integer
        num2 = read x :: Integer
        res = cacl_value xs ys + (num1 * num2)

length1 :: Num a1 => [a2] -> a1
length1 xs = len xs 0 where  
              len []     acc = acc
              len (x:xs) acc = len xs $! (1 + acc)

sum_ :: IO()
sum_ = do
    let
    -- чтение рецептов
    recipe_handle <- openFile "new_recept.txt" ReadMode
    recipe_contents <- hGetContents recipe_handle
    -- чтение ингредиенты
    ingredient_handle <- openFile "new_ing.txt" ReadMode
    ingredient_contents <- hGetContents ingredient_handle
    -- чтение цен за ингредиенты
    price_handle <- openFile "price.txt" ReadMode
    price_contents <- hGetContents price_handle
    -- чтение цен за услугу
    work_handle <- openFile "work_price.txt" ReadMode
    work_contents <- hGetContents work_handle


    let
        -- получение всех рецептов из файла
        all_recept = wordsWhen (==' ') recipe_contents
        recept_struct = map add_stuct all_recept

    print "Choose recept"
    mapM_ print_ recept_struct
    choose <- getLine
    

    let
        -- получение всех ингредиентов указанного рецепта
        -- разбиваем список по переносу строки
        list_ingredient = wordsWhen (=='\n') ingredient_contents
        --разбиваем еще каждый список слова по пробелу
        all_ingredient =  map (wordsWhen (==' '))  list_ingredient
        -- все это в структуру данных
        struct_ingredient = map add_ing  all_ingredient
        -- поиск нужного рецепта
        find_ingredient = map (find_ choose) struct_ingredient
        -- очистка от мусора
        clear_ingredient = catMaybes find_ingredient
    if clear_ingredient == [] then print "Recept not found"
    else do
        let
            -- получение  списка  ингредиентов из структуры ингредиентов
            get_ingredients = map get_ingredient clear_ingredient
            -- получение списка грамовок ингредиентов
            get_value_ingredients = map get_value_ingredient clear_ingredient

            -- разбиваем список по переносу строки
            list_price = wordsWhen (=='\n') price_contents
            --разбиваем еще каждый список слова по пробелу
            all_price = map (wordsWhen (==' '))  list_price
            -- все это в структуру данных
            stuct_price = map add_price all_price
            -- получение список стоимости
            need_price = map (get_price stuct_price) get_ingredients
            -- отчистка от мусора
            clear_price = catMaybes need_price

            -- получаем список цен за грамм продукта
            price_value = map get_price_value clear_price
            ing_error = map get_ing clear_price
        if length1 price_value /= length1 get_value_ingredients then do 
            print "Error need add price ingredient. ingredient show >"
            print ing_error
            print "Recept show>"
            print get_ingredients
        else do
            let
                -- считаем стоимость рецепта
                calc_price = cacl_value price_value get_value_ingredients

                list_work = wordsWhen (=='\n') work_contents
                --разбиваем еще каждый список слова по пробелу
                all_work = map (wordsWhen (==' '))  list_work
                -- все это в структуру данных
                stuct_work = map add_work all_work
                -- получение список стоимости
                find_works = map (find_work choose) stuct_work
                clear_find_works = catMaybes find_works

                result_sum = calc_result calc_price (head clear_find_works)

                --итоговый прайс
            print("Result sum= " ++ show result_sum)




    --mapM_

