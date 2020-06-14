

make_list :: [Integer] -> [Integer]
make_list list = [ x | x <- [1..30] , mod x 2 == 0]

main :: IO ()
main = print (make_list [1,2,3,4,5,6,7,8,9,10])