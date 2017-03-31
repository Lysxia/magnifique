data Tree = Node Tree Tree | Leaf Int
  deriving Show

fib :: Int -> Int -> Tree
fib m n | m < 2 = Leaf n
fib m n = Node (fib (m-1) (n+1)) (fib (m-2) (n+2))

main = print (fib 5 0)
