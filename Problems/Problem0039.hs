import Utils.ListOps (maximumUsing)

main = print problem39Value

problem39Value :: Integer
problem39Value = maximumUsing rightTriangleSolutions [1..1000]

rightTriangleSolutions :: Integer -> Int
rightTriangleSolutions p = length $ [(a,b,c) | a <- [1..p], b <- [a+1..p], c <- [p-a-b], a<b && b<c, a+b+c == p, a^2+b^2 == c^2]
