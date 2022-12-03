import Data.List (sort)

main = do
    calorieString <- readFile "./calories.txt"
    let calorieStringLines = lines calorieString
    let groupedCalorieStrings = groupCaloriesByEmptyLines calorieStringLines
    let elfInventories = map parseStringsToInventory groupedCalorieStrings
    let maxCalorieInventory = highestTotalCalorieInInventory elfInventories
    print maxCalorieInventory
    let top3CalorieInventory = top3TotalCalorieInInventory elfInventories
    print top3CalorieInventory

newtype Consumable = Consumable 
    {
        calories :: Int
    } 

newtype Inventory = Inventory 
    {
        consumables :: [Consumable]
    }

groupCaloriesByEmptyLines :: [String] -> [[String]]
groupCaloriesByEmptyLines xs = 
    let helper :: [String] -> [String] -> [[String]]
        helper [] acc = [acc]
        helper ("":xs) acc = acc : helper xs []
        helper (x:xs) acc = helper xs (acc ++ [x])
    in helper xs []

parseStringsToInventory :: [String] -> Inventory
parseStringsToInventory [] = Inventory []
parseStringsToInventory xs = Inventory [Consumable (read x) | x <- xs]

highestTotalCalorieInInventory :: [Inventory] -> Int
--goal: list of sum of list of calories
highestTotalCalorieInInventory xs = maximum $ map sum $ (map . map) calories inventoryConsumables
    where
        inventoryConsumables = map consumables xs

top3TotalCalorieInInventory :: [Inventory] -> Int
top3TotalCalorieInInventory xs = sum $ take 3 $ reverse $ sort $ map sum $ (map . map) calories inventoryConsumables
    where
        inventoryConsumables = map consumables xs
