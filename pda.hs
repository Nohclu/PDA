type Transitiion = ((Int, String, String), (Int, String))

type Configuration = (Int, String, String)

type PDA = (Int, [Int], [Transitiion])

-- data Result = Accept | Reject deriving Show -- String = Input string

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

-- -- run :: PDA -> String -> Result

steps :: Configuration -> [Transitiion] -> [Configuration]
steps (a,b,c) [((d,[e],[f]),(g,[h]))]