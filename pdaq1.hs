type Transitiion = ((Int, String, String), (Int, String))

type Configuration = (Int, String, String)

type PDA = (Int, [Int], [Transitiion])

data Result = Accept | Reject deriving Show -- String = Input string

steps :: Configuration -> [Transitiion] -> [Configuration] -> [Configuration]
steps c [] cs = cs
steps (a,"",c) tr cs = cs
steps (a,b,c) (t:tr) cs = steps (a,b,c) tr (cs ++ (step(a,b,c)t))

step :: Configuration -> Transitiion -> [Configuration]
--Rule1
step (a,b,"") ((d,"",""),(g,""))
  |a == d = [(g,b,"")]
  |otherwise = []

--Rule2
step (a,b,"") ((d,"",""),(g,[h]))
  |a==d = [(g,b,[h])]
  |otherwise = []

--Rule4
step (a,(b:bs),"") ((d,[e],""),(g,""))
  |a==d && b==e = [(g,bs,"")]
  |otherwise = []

--Rule6
step (a,(b:bs),"") ((d,[e],""),(g,[h]))
  |a==d && b==e = [(g,bs,[h])]
  |otherwise = []

--Rule9
step (a,b,c) ((d,"",""),(g,""))
  |a==d = [(g,b,c)]
  |otherwise = []

--Rule10
step (a,b,c) ((d,"",""),(g,[h]))
  |a==d = [(g,b,(h:c))]
  |otherwise = []

--Rule11
step (a,b,(c:cs)) ((d,"",[f]),(g,""))
  |a==d && c==f = [(g,b,cs)]
  |otherwise = []

--Rule12
step (a,b,(c:cs)) ((d,"",[f]),(g,[h]))
  |a==d && c==f = [(g,b,(h:cs))]
  |otherwise = []

--Rule13
step (a,(b:bs),c) ((d,[e],""),(g,""))
  |a==d && b==e = [(g,bs,c)]
  |otherwise = []

--Rule14
step (a,(b:bs),c) ((d,[e],""),(g,[h]))
  |a==d && b==e = [(g,bs,h:c)]
  |otherwise = []
--Rule15
step (a,(b:bs),(c:cs)) ((d,[e],[f]),(g,""))
    | a==d && b==e && c==f = [(g,bs,cs)]
    | otherwise = []

--Rule16
step (a,(b:bs),(c:cs)) ((d,[e], [f]), (g, [h]))
    | a==d && b==e && c==f = [(g,bs,(h:cs))]
    | otherwise = []

step (a,b,"") ((d,"",[f]),(g,"")) = []
step (a,b,"") ((d,"",[f]),(g,[h])) = []
step (a,b,"") ((d,[e],[f]),(g,"")) = []
step (a,b,"") ((d,[e],[f]),(g,[h])) = []
-- -- run :: PDA -> String -> Result

finalState :: Configuration -> [Int] -> Bool
finalState (a,b,c) [] = False
finalState (a,b,c) (s:ss)
  | a == s && b == "" && c == "" = True
  | otherwise = finalState (a,b,c) ss

runPrime :: PDA -> [Configuration] -> Result
runPrime (i,s,tr) [] = Reject
runPrime (i,s,tr) (x:xs)
  | finalState x s == True = Accept
  | otherwise = runPrime(i,s,tr) (xs ++ (steps x tr []))


run :: PDA -> String -> Result
run (i, s, tr) "" = Accept
run (i, s, tr) input = runPrime (i, s, tr) [(i, input, "")]

