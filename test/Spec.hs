import Lib(isSorted, multiplyMod)
import Test.QuickCheck.Test

prop_multiplyMod :: Int -> Int -> Int -> Bool
prop_multiplyMod x y m | m > 0 = (multiplyMod x y m) `mod` m == (x * y) `mod` m
                       | otherwise = True

prop_neutralElement :: Int -> Int -> Bool
prop_neutralElement x m   | m > 0 = multiplyMod x 1 m == x `mod` m
                          | otherwise = True

prop_commutativity :: Int -> Int -> Int -> Bool
prop_commutativity x y m | m > 0 = multiplyMod x y m == multiplyMod y x m 
    | otherwise = True

prop_oneMemberList ::  (Ord a) => a -> Bool -> Bool
prop_oneMemberList member = isSorted [member]

prop_2elementsTrue ::  Ord a => a -> a -> Bool
prop_2elementsTrue x y  = isSorted [x,y] True == (x<=y)

prop_2elementsFalse ::  Ord a => a -> a -> Bool
prop_2elementsFalse x y  = isSorted [x,y] False == (x>=y)

prop_elemenateElement :: Ord a => [a] -> Bool -> Bool
prop_elemenateElement list direction | isSorted list direction =  all  (\x -> isSorted (filter (/=x) list) direction) list
                                     |otherwise = True

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
    putStrLn "\n\n---Тестирования проверки сортировки---"
    putStrLn "-Тест №1 Проверка списка состоящего из одного элемента-"
    quickCheckWith stdArgs {maxSuccess = 1000} 
        (prop_oneMemberList:: Int -> Bool -> Bool)
    putStrLn "-Тест №2 Проверка списка, состоящего из двух элементов на сортировку по возрастанию-"
    quickCheckWith stdArgs {maxSuccess = 1000} 
        (prop_2elementsTrue:: Int -> Int -> Bool)
    putStrLn "-Тест №3 Проверка списка, состоящего из двух элементов на сортировку по убыванию-"
    quickCheckWith stdArgs {maxSuccess = 1000} 
        (prop_2elementsFalse:: Int -> Int -> Bool)
    putStrLn "-Тест №4 Проверка сохранения сортировки при удалении элмента из списка-"
    quickCheckWith stdArgs {maxSuccess = 1000} 
        (prop_elemenateElement:: [Int] -> Bool -> Bool)
        
    putStrLn "Готово!"
