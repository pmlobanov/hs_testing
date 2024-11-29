import Lib (multiplyMod)
import Data.Maybe
import Test.QuickCheck.Test

prop_multiplyMod :: Int -> Int -> Int -> Bool
prop_multiplyMod x y m | m == 0 = isNothing (multiplyMod x y m)
                       | otherwise = if isNothing result then False 
                                     else fromJust result == (x * y) `mod` m
                                     where result = multiplyMod x y m

prop_neutralElement :: Int -> Int -> Int -> Bool
prop_neutralElement x _ m   | m == 0 = isNothing (multiplyMod x 1 m) 
                            | otherwise = if isNothing result then False 
                                          else fromJust result == x `mod` m
                                          where result = multiplyMod x 1 m

prop_commutativity :: Int -> Int -> Int -> Bool
prop_commutativity x y m = multiplyMod x y m == multiplyMod y x m 


main :: IO ()
main = do 
    putStrLn "---Тестирования функции умножения по модулю---"
    putStrLn "-Тест №1 Проверка правильности умножения-"
    quickCheckWith stdArgs {maxSuccess = 1000} 
        prop_multiplyMod
    putStrLn "-Тест №2 Проверка нейтрального элемента-"
    quickCheckWith stdArgs {maxSuccess = 1000} 
       prop_neutralElement 
    putStrLn "-Тест №3 Проверка коммутативности-"
    quickCheckWith stdArgs {maxSuccess = 1000} 
        prop_commutativity
    putStrLn "Готово!"
