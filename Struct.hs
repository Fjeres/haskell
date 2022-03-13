module Struct where


data Recipe = Recipe  {
    recipe  :: String
} deriving (Eq, Show, Read)

data PriceW = PriceW  {
    recipeW :: String,
    priceW :: String
} deriving (Eq, Show, Read)

data Ingredient = Ingredient  {
    ingredient :: String,
    recipe_ :: String,
    value :: String
} deriving (Eq, Show, Read)

data Price = Price  {
    ingredient_ :: String,
    price :: String
} deriving (Eq, Show, Read)

