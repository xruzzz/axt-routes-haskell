{-# LANGUAGE UnicodeSyntax #-}
module Main where

--import Graph(Edge(..), Node(..),DefaultNode)
-- import qualified MidGraph as OG
import Control.Monad
import Text.Read
-- Модуль Graph


data Edge a b = Ed a b deriving (Show, Read, Eq)
data EdgeR a b = EdR (b, NodeR a b, NodeR a b) deriving (Show, Read, Eq)
data Node a b = Vt a b b [Edge a b] deriving (Show, Read, Eq)
data NodeR a b = NodeR {  name :: a, x,y ::b } deriving (Show, Read, Eq)
type DefaultNode = Node String Double
{-type DNR = NodeR String Double
type ER = EdgeR String Double
type Graph = [DefaultNode]
type GraphR = ([DNR],[ER])

dist ∷ DefaultNode → DefaultNode → Double
dist (Vt n1 x1 y1 xs1) (Vt n2 x2 y2 xs2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

distR ∷ DNR → DNR → Double
distR (NodeR {name = n1, x = x1, y = y1}) (NodeR {name = n2, x = x2, y = y2}) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2
  -}
-- Определение графа
graph1 = [
                (Vt "A" 0 0 [(Ed "B" 2), (Ed "C" 4)]),
                (Vt "B" 1 1 [(Ed "A" 2), (Ed "C" 2), (Ed "D" 3)]),
                (Vt "C" 2 2 [(Ed "A" 4), (Ed "B" 2), (Ed "D" 2)]),
                (Vt "D" 1 3 [(Ed "B" 3), (Ed "C" 2)]),
                (Vt "E" 0 0 [])
                ]

-- Описание интерфейса программы
lru 1 = "Нахождение кратчайшего пути в графе v0.1.0.1"
lru 2 = "Начальная вершина"
lru 3 = "Конечная вершина"
lru 4 = "Максимально допустимая стоимость пути"
lru 5 = "Самый короткий маршрут"

ads ∷ Int → String
ads i = lru i ++ ":"

ad' ∷ Int → String
ad' 
(≡) ∷ (Eq a)⇒ a→a→String
(≡) a1 a2
  | a1 == a2 = ""
  | otherwise = " не"

tests ∷ IO ()
tests = do
  putStrLn $ "Тест 1" ++ ((shortestPath graph1 "A" "D" 40) ≡ ["A","B","D"]) ++ " пройден"

ls = ["началь","конеч"]
txend = "ную вершину:"
-- Возвращает True if string matches the Node' name
samename ∷ String → DefaultNode → Bool
samename n (Vt name _ _ _) = if (n == name) then True else False

main ∷ IO ()
main = do
  putStrLn $ lru 1
  print graph1
  putStrLn $ 
  putStrLn $ "  - "++ lru 2 ++":"
  n1 ← getLine
  putStrLn $ "  - "++ lru 3 ++":"
  n2 ← getLine
  let nc1 = not $ null (filter (samename n1) graph1)
  let nc2 = not $ null (filter (samename n1) graph1)
  if (nc1 && nc2) then do
    putStrLn $ "  - " ++ lru 4 ++":"
    s ← getLine
    let res = reads s::[(Double, String)]
        dd = fst $ head res
    if (length res > 0) then do
            putStrLn "Самый короткий маршрут:"
            print $ (shortestPath graph1 n1 n2 dd)
    else
        putStrLn "Неверно указана допустимая стоимость"
  else 
    putStrLn "Неверно указаны вершины"

-- Модуль MidGraph

-- Data types

data Path = Pt [String] Double
data Node' = Vtx (DefaultNode) Path Bool
type Nodelist = [DefaultNode]
data Mainlist = Mls [Node'] Bool

-- Возвращает a path
shortestPath ∷ Nodelist → String → String → Double → [String]
shortestPath nodes start goal maxcost = 
  reverse (printpath (expansionblock nodes (Mls [(Vtx (getvert nodes start) (Pt [start] 0) False)] True) (Vtx (getvert nodes goal) (Pt [] 0) False) maxcost) (getvert nodes goal))

-- Prints a vertex name (ToString())
printvert ∷ Node' → String
printvert (Vtx (Vt n _ _ _) _ _) = n

-- Вывести маршрут
printpath ∷ Mainlist → DefaultNode → [String]
printpath m goal | notreached (Vtx goal (Pt [] 0) False) m = ["Нет доступного маршрута!"]
printpath (Mls vs _) goal = getpath (getvert' vs (printvert (Vtx goal (Pt [] 0) False)))
  where getpath ∷ Node' → [String]
        getpath (Vtx _ (Pt p _) _) = p

-- Block function
-----------------

-- Block for expanding nodes, verifies against matches and updates as needed
expansionblock ∷ Nodelist → Mainlist → Node' → Double → Mainlist
expansionblock nodes (Mls open changed) goal cost =
                let minv = (getminvert open goal cost)
                in if (isworthexpanding minv goal cost) && changed
                                then (expansionblock nodes (addexpanded (Mls (markexpanded open minv) False) (expandall nodes minv)) goal (getnewcost open (printvert goal) cost))
                                else (Mls open False)
    where
        -- Udates list, adding expanded nodes and checking distances
        addexpanded ∷ Mainlist → [Node'] → Mainlist
        addexpanded m [] = m
        addexpanded m (e:exp) = addexpanded (cheaperpath m e) exp
        -- Возвращает new cost after a cycle, leaves intact if no path found
        getnewcost ∷ [Node'] → String → Double → Double
        getnewcost [] _ x = x
        getnewcost ((Vtx (Vt name _ _ _) (Pt p x) _): vs) name1 oldcost | name1 == name && x < oldcost = getnewcost vs name1 x
        getnewcost (_ : vs) vert oldcost = getnewcost vs vert oldcost

-- Replace a vertex path by a cheaper path if available
cheaperpath ∷  Mainlist → Node' →  Mainlist
cheaperpath (Mls [] _) e = (Mls [e] True)
cheaperpath (Mls (v:vx) b) e | (printvert e) == (printvert v) =
                if (getcheapest e v) then (Mls (e:vx) True) else (Mls (v:vx) b)
cheaperpath (Mls (v:vx) b) e = addtomainlist (cheaperpath (Mls vx b) e) v
 
-- Main list related functions
------------------------------

-- Возвращает vertex по его имени
getvert ∷ Nodelist → String → DefaultNode
getvert ((Vt name x y e) : vs) name1 | name1 == name = (Vt name x y e)
getvert (_ : vs) name = getvert vs name
getvert [] s2 = error $ "Ошибка "++ s2

getvert' ∷ [Node'] → String → Node'
getvert' (v : vs) name | (printvert v) == name = v
getvert' (_ : vs) name = getvert' vs name

-- Возвращает a vertex with minimal g + h
getminvert ∷ [Node'] → Node' → Double → Node'
getminvert (v:vs) ve vx | not(notexpanded v) = getminvert vs ve vx
getminvert (v:vs) ve vx = foldl (\acc cur → if (totalcost cur ve) < (totalcost acc ve) && (notexpanded cur) then cur else acc) v vs
 
-- Expand nodes, returns a list of new nodes
expandall ∷ Nodelist → Node' → [Node']
expandall vs (Vtx (Vt a b c e) p r) = map (expandone vs (Vtx (Vt a b c e) p r)) e
  where
      expandone ∷ Nodelist → Node' → Edge String Double→ Node'
      expandone vs (Vtx _ (Pt p x) _) (Ed n c) = (Vtx (getvert vs n) (Pt (n:p) (c + x)) False)

-- Udates list, setting present node to expanded
markexpanded ∷ [Node'] → Node' → [Node']
markexpanded (v:exp) v1 | (printvert v) == (printvert v1) = ((setexpanded v):exp)
  where
      setexpanded ∷ Node' → Node'
      setexpanded (Vtx v p r) = (Vtx v p True)
markexpanded (e:exp) v = (e:(markexpanded exp v))

 
-- Возвращает a Node' list from a Mainlist
addtomainlist ∷ Mainlist → Node' →  Mainlist
addtomainlist (Mls vs b) v = (Mls (v:vs) b)

-- Name filter predicates
-------------------------

-- Возвращает True if string matches the Node' name
samename' ∷ String → Node' → Bool
samename' n1 n = if (n1 == (printvert n)) then True else False
 
-- Возвращает True if string does not match the Node name
diffname' ∷ String → Node' → Bool
diffname' a b = not (samename' a b)

-- Возвращает True if node has not been reached yet
notreached ∷ Node' → Mainlist → Bool
notreached v (Mls open _) = (null(filter (samename' (printvert v)) open))

-- Возвращает True if node has not been expanded yet
notexpanded ∷ Node' → Bool
notexpanded (Vtx _ (Pt p _) b) = (not b)

-- Cost-related functions
-------------------------

-- Возвращает g(current) + h(current, goal)
totalcost ∷ Node' → Node' → Double
totalcost a b = (cost a) + (dist a b)
  where dist ∷ Node' → Node' → Double -- Возвращает the value of h(current, goal)
        dist (Vtx (Vt _ x1 y1 _) _ _) (Vtx (Vt _ x2 y2 _) _ _) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
        -- Возвращает the value of g(current)
        cost ∷ Node' → Double
        cost (Vtx _ (Pt _ x) _) = x

-- Возвращает true if the first vertex is cheaper than the second
getcheapest ∷ Node' → Node' → Bool
getcheapest (Vtx _ (Pt _ x) _) (Vtx _ (Pt _ y) _) = if x < y then True else False

-- Tests if the vertex is potentially worth expanding
isworthexpanding ∷ Node' → Node' → Double → Bool
isworthexpanding a b c = if (totalcost a b) < c then True else False
