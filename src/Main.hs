module Main where

-- Main module
import System.IO
import System.Directory (getDirectoryContents)

import PGN
  
gameDefinition = "[Event \"Cap d'Agde-B\"]\n[Site \"?\"]\n[Date \"2006.10.27\"]\n[Round \"?\"]\n[White \"Karpov, Anatoly\"]\n[Black \"Carlsen, Magnus\"]\n[ECO \"A05\"]\n[WhiteElo \"2668\"]\n[BlackElo \"2698\"]\n[Result \"1/2-1/2\"]\n\n1. Nf3 Nf6 2. c4 c5 3. g3 g6 4. Bg2 Bg7 5. O-O Nc6 6. d4 cxd4 7. Nxd4 O-O 8. Nc3 Ng4 9. e3 d6 10. h3 Nge5 11. b3 Nxd4 12. exd4 Nc6 13. Be3 Bd7 14. Qd2 Qa5 15. Rfd1 Rac8 16. Rac1 Rfe8 17. d5 Nb4 18. Nb1 b6 19. a3 Na6 20. b4 Qa4 21. Bd4 Bxd4 22. Qxd4 b5 23. Qxa7 Nc5 24. Qxa4 Nxa4 25. cxb5 Nb2 26. Re1 Nd3 27. Rxc8 Rxc8 28. Rd1 Nb2 29. Re1 Nd3 30. Rd1 \n"
gamedata = extractGamedata gameDefinition


main = do
  putStrLn "Beginning..."