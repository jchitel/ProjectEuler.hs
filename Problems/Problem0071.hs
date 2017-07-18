import Utils.ListOps (minimumUsing)
import Utils.IntegerOps (divide)

main = print problem71Value

problem71Value :: Int
problem71Value = (`quot` 7) $ (*3) $ minimumUsing (\x -> ((x*3) `divide` 7) - (fromIntegral (x*3 `quot` 7))) $ filter (\x -> x `rem` 7 /= 0) [5..1000000]
