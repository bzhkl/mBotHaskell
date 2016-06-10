import MBotLibrary

main = do
  d <- openMBot
  forward 60 d
  wait 5
  backward 60 d
