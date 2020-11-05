--produto tem id, nome, preço, sintomas e data de validade
--atualizar preço de um produto permitindo que descontos sejam aplicados
--visualizar lista de produtos (produtos existentes no estoque e produtos de acordo com os sintomas)

data Produto = Produto {
    idProduto :: Int,
    nomeProduto :: String,
    preco :: Double,
    sintomasProduto :: [String],
    validade :: String
} deriving (Show, Read)

data Produtos = Produtos {
    produtos :: [(Int, Produto)]
} deriving Show

getIdProduto :: Produto -> Int
getIdProduto Produto {idProduto = i} = i

getNomeProduto :: Produto -> String
getNomeProduto Produto {nomeProduto = n} = n

getPreco :: Produto -> Double
getPreco Produto {preco = p} = p 

getSintomasProduto :: Produto -> [String]
getSintomasProduto Produto {sintomasProduto = s} = s

getValidade :: Produto -> String
getValidade Produto {validade = v} = v

setPreco :: [Produto] -> Int -> Double -> Maybe [Produto]
setPreco [] x novoPreco = Nothing
setPreco (c:cs) x novoPreco
    | idAtual == x = Just ([Produto x nomeProdutoAtual novoPreco sintomasProdutoAtual validadeAtual] ++ cs)
    | otherwise = setPreco cs x novoPreco
    where
        idAtual = getIdProduto c
        nomeProdutoAtual = getNomeProduto c
        sintomasProdutoAtual = getSintomasProduto c
        validadeAtual = getValidade c

produtoToString :: Produto -> String
produtoToString Produto {idProduto = i, nomeProduto = n, preco = p, sintomasProduto = s, validade = v} = show i ++"," ++ n ++ "," ++ show p ++ "," ++ sintomasToString s ++ "," ++ v

produtosToString :: [Produto] -> String
produtosToString [] = []
produtosToString (p:ps) = if length ps > 0 then do "["++produtoToString p ++"]," ++ produtosToString ps
    else do "[" ++ produtoToString p ++ "]"

sintomasToString :: [String] -> String
sintomasToString [] = []
sintomasToString (s:sw) = s ++ sintomasToString sw
