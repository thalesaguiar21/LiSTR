tokenizer :: String -> [String]
tokenizer [] = "E":[]
tokenizer (h:t) = if h `elem` listaDeliminatores then (h:[]):tokenizer t
                      else let ((h2:h1):t1) = tokenizer t
                           in
                        if h2 `elem` listaDeliminatores then (h:[]):(h2:h1):(t1)
                            else (h:(h2:h1)):t1
                where listaDeliminatores = ['+','-','(',')']