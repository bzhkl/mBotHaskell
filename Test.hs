import qualified Data.Map.Strict as Map

testMap :: Map.Map String Int
testMap = Map.fromList [("_MBOT", 57), ("test", 5), ("huis", 23), ("_krapuul", 18)]

main :: IO ()
main = do
  let f = Map.filterWithKey (\k _ -> head k == '_') testMap
  print f
