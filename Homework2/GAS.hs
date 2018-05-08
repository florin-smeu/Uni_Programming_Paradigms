{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M
import qualified Data.Char as C
import Data.Maybe

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord)

instance Show Color where
    show Red = "r"
    show Blue = "b"
    show Gray = "g"    

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Object = Square {color :: Color, heading :: Heading} | Circle {color :: Color} | Arrow {heading :: Heading}
    deriving (Eq, Ord)

{-
    Reprezetarea textuală a unui obiect.
-}
instance Show Object where
    show (Square {color = c, heading = h}) = (map C.toUpper $ show c) ++ (show h)  
    show (Circle {color = c}) = show c 
    show (Arrow {heading = h}) = show h

{-
    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}

data Level = FullLevel {mapping :: M.Map Position [Object]}
    deriving (Eq, Ord)   


{- 
    Functii helper care returneaza coordonatele minime si maxime ale matricei de pozitii.
-}
getXMin :: M.Map Position [Object] -> Int
getXMin m = foldl (\acc e -> if fst (fst e) < acc then fst (fst e) else acc) (maxBound :: Int) $ M.toList m

getXMax :: M.Map Position [Object] -> Int
getXMax m = foldl (\acc e -> if fst (fst e) > acc then fst (fst e) else acc) (minBound :: Int) $ M.toList m

getYMin :: M.Map Position [Object] -> Int
getYMin m = foldl (\acc e -> if snd (fst e) < acc then snd (fst e) else acc) (maxBound :: Int) $ M.toList m

getYMax :: M.Map Position [Object] -> Int
getYMax m = foldl (\acc e -> if snd (fst e) > acc then snd (fst e) else acc) (minBound :: Int) $ M.toList m


{- 
    Functie helper care are rolul de a crea (sub forma unei liste) toate pozitiile 
    din matricea care va fi afisata.
-}
createPositions :: M.Map Position [Object] -> [Position]
createPositions m = [(x,y) | x <- [getXMin m..getXMax m], y <- [getYMin m..getYMax m]]

{- 
    Functia printObject realizeaza afisarea explicita a unui obiect de la o anumita pozitie.
-}
printObject :: [Object] -> String
printObject objects 
    | numberOfObjects == 0 = "   "
    | numberOfObjects == 1 = if C.isUpper $ head $ show $ head objects 
                                 then (show $ head objects) ++ " "
                                 else "  " ++ (show $ head objects)
    | otherwise = if C.isUpper $ head $ show $ head objects
                     then (show $ head objects) ++ (show $ last objects)
                     else (show $ last objects) ++ (show $ head objects) 
    where numberOfObjects = length objects

{- 
    Functia print afiseaza continutul corespunzator unei pozitii din matricea de joc.
-}
printPosition :: String -> Maybe [Object] -> String
printPosition endChar objects = 
    if isNothing objects
        then "   " ++ endChar 
        else (printObject (fromJust objects)) ++ endChar    

{-
    Reprezetarea textuală a unui nivel.
-}
instance Show Level where
    show (FullLevel {mapping = m}) = foldl (\acc pos -> if pos == (getXMax m, getYMax m) 
                                                           then acc ++ (printPosition "" $ M.lookup pos m)
                                                           else if snd pos == getYMax m
                                                                   then acc ++ (printPosition "\n" $ M.lookup pos m)
                                                                   else acc ++ (printPosition "|" $ M.lookup pos m)
                                           ) "" $ createPositions m     

{-  
    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = FullLevel M.empty

{-
    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare c h pos (FullLevel {mapping = m}) = FullLevel $ M.insertWith (++) pos [square] m    
    where square = Square {color = c, heading = h}

{-
    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle c pos (FullLevel {mapping = m}) = FullLevel $ M.insertWith (++) pos [circle] m
    where circle = Circle {color = c}

{-
    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow h pos (FullLevel {mapping = m}) = FullLevel $ M.insertWith (++) pos [arrow] m
    where arrow = Arrow {heading = h}

{-  
    Functii helper care returneaza culoarea si headingul unui obiect, sau caracterul 
    care reprezinta acest lucru din reprezentarea lor textuala.  
-}
getHChar :: Object -> Char 
getHChar obj
    | C.isUpper firstLetter = last $ show obj
    | otherwise = firstLetter
    where firstLetter = head $ show obj

getHeading :: Object -> Heading
getHeading obj 
    | h == '^' = North
    | h == '>' = East
    | h == 'v' = South
    | otherwise = West
    where h = getHChar obj 

getCChar :: Object -> Char 
getCChar obj = head $ show obj

getColor :: Object -> Color
getColor obj 
    | c == 'B' || c == 'b' = Blue
    | c == 'G' || c == 'g' = Gray
    | otherwise = Red
    where c = getCChar obj

{-
    Functie helper care verifica daca un patrat este mutat pe o pozitie la care se
    afla deja o sageata si returneaza headingul corect al patratului.
-}    
updateHeading :: Heading -> Position -> Level -> Heading
updateHeading oldHeading pos (FullLevel {mapping = m}) = 
    case M.lookup pos m of
        Nothing -> oldHeading
        Just [obj] -> if C.isLetter $ getCChar obj
                         then oldHeading
                         else getHeading obj
        Just [obj1,obj2] -> if C.isLetter $ getCChar obj1
                               then getHeading obj2
                               else getHeading obj1
        Just _ -> oldHeading                       
                                                     	                	    	    	     

{-
    Functie care sterge din map patratul de la pozitia anterioara mutarii.
-}
deleteOldSquare :: Position -> Level -> Level
deleteOldSquare pos (FullLevel {mapping = m}) = 
    case M.lookup pos m of
    	Nothing -> FullLevel m
        Just [_] -> FullLevel $ M.delete pos m
        Just [obj1, obj2] -> if C.isUpper $ getCChar obj1
                                then if C.isLetter $ getCChar obj2
                                        then addCircle (getColor obj2) pos $ FullLevel $ M.delete pos m
                                        else addArrow (getHeading obj2) pos $ FullLevel $ M.delete pos m
                                else if C.isLetter $ getCChar obj1
                                        then addCircle (getColor obj1) pos $ FullLevel $ M.delete pos m
                                        else addArrow (getHeading obj1) pos $ FullLevel $ M.delete pos m 
        Just _ -> FullLevel m                                    

{-
    Functia pushSquare va realiza impingerea recursiva a patratelor ca urmare a mutarii unui
    patrat intr-o pozitie in care se afla deja un patrat.	
-}
pushSquare :: Heading -> Color -> Heading -> Position -> Level -> Level
pushSquare oldHeading c h pos (FullLevel {mapping = m})
    | oldHeading == North = addSquare c h (x - 1, y) $ deleteOldSquare pos $ push oldHeading (x - 1, y) $ FullLevel m
    | oldHeading == East = addSquare c h (x, y + 1) $ deleteOldSquare pos $ push oldHeading (x, y + 1) $ FullLevel m
	| oldHeading == South = addSquare c h (x + 1, y) $ deleteOldSquare pos $ push oldHeading (x + 1, y) $ FullLevel m
    | otherwise = addSquare c h (x, y - 1) $ deleteOldSquare pos $ push oldHeading (x, y - 1) $ FullLevel m
    where x = fst pos
          y = snd pos

{- 
   Functia push este folosita pentru a verifica daca mai este necesar sa fie impinse patrate noi.
   In caz afirmativ, se apeleaza functia pushSquare, care realizeaza explicit acest lucru.  
-}
push :: Heading -> Position -> Level -> Level
push oldHeading pos (FullLevel {mapping = m}) = 
    case M.lookup pos m of
        Nothing -> FullLevel m 
        Just [obj] -> if C.isUpper $ getCChar obj
          	             then pushSquare oldHeading (getColor obj) (getHeading obj) pos $ FullLevel m
          	             else FullLevel m
        Just [obj1,obj2] -> if C.isUpper $ getCChar obj1
                               then if C.isLetter $ getCChar obj2
                                       then pushSquare oldHeading (getColor obj1) (getHeading obj1) pos $ FullLevel m
                                       else pushSquare oldHeading (getColor obj1) (getHeading obj2) pos $ FullLevel m
                               else if C.isLetter $ getCChar obj1
                                       then pushSquare oldHeading (getColor obj2) (getHeading obj2) pos $ FullLevel m
                                       else pushSquare oldHeading (getColor obj2) (getHeading obj1) pos $ FullLevel m
        Just _ -> FullLevel m 

{-
    Functia moveSquare realizeaza mutarea explicita a unui patrat de la o anumita pozitie,
    in conformitate cu headingul acestuia.
-}
moveSquare :: Color -> Heading -> Position -> Level -> Level
moveSquare c h pos (FullLevel {mapping = m})
    | h == North = addSquare c n (x - 1, y) $ deleteOldSquare pos $ push n (x - 1, y) $ FullLevel m                                               
    | h == East = addSquare c e (x, y + 1) $ deleteOldSquare pos $ push e (x, y + 1) $ FullLevel m
    | h == South = addSquare c s (x + 1, y) $ deleteOldSquare pos $ push s (x + 1, y)  $ FullLevel m
    | otherwise = addSquare c w (x, y - 1) $ deleteOldSquare pos $ push w (x, y - 1) $ FullLevel m
    where x = fst pos
          y = snd pos
          n = updateHeading North (x - 1, y) $ FullLevel m
          e = updateHeading East (x, y + 1)  $ FullLevel m 
          s = updateHeading South (x + 1, y) $ FullLevel m
          w = updateHeading West (x, y - 1) $ FullLevel m

{-
    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}
move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
move pos (FullLevel {mapping = m}) = 
	case M.lookup pos m of
        Nothing -> FullLevel m 
        Just [obj] -> if C.isUpper $ getCChar obj
          	             then moveSquare (getColor obj) (getHeading obj) pos $ FullLevel m
          	             else FullLevel m
        Just [obj1,obj2] -> if C.isUpper $ getCChar obj1
                               then if C.isLetter (getCChar obj2)
                                       then moveSquare (getColor obj1) (getHeading obj1) pos $ FullLevel m
                                       else moveSquare (getColor obj1) (getHeading obj2) pos $ FullLevel m
                               else if C.isLetter (head (show obj1))
                                       then moveSquare (getColor obj2) (getHeading obj2) pos $ FullLevel m
                                       else moveSquare (getColor obj2) (getHeading obj1) pos $ FullLevel m 
        Just _ -> FullLevel m                               


{-
     Functie helper care verifica daca la o obiectele de la o anumita pozitie contin un patrat.	
-}
containsSquare :: [Object] -> Bool
containsSquare objects
    | n == 0 = False
    | n == 1 = if C.isUpper $ getCChar $ head objects
   	             then True
   	             else False
    | n == 2 = if C.isUpper $ getCChar $ head objects
                 then  True
                 else if C.isUpper $ getCChar $ last objects
                      then True
                      else False
    | otherwise = False
    where n = length objects                      	               

{-
    Functie care returneaza pozitiile tuturor patratelor dintr-un nivel la un momentdat.
-}
allSquares :: Level -> [Position]
allSquares (FullLevel {mapping = m}) = foldr (\e acc -> if containsSquare (snd e)
                                                           then (fst e) : acc
                                                           else acc  
                                             ) [] (zip (M.keys m) (M.elems m))

{-
    Functia verifica daca obiectele de la o anumita pozitie sunt intr-o configuratie 
    castigatoare sau nu.
-}
isGoalObject :: [Object] -> Bool
isGoalObject objects 
	| n == 0 = True
	| n == 1 = if C.isUpper $ getCChar $ head objects
	              then False
	              else True
	| n == 2 = if C.isUpper $ getCChar $ head objects
	              then if C.toUpper (getCChar $ last objects) == (getCChar $ head objects)
	                      then True
	                      else False
	              else if C.isUpper $ getCChar $ last objects
	                      then if C.toUpper (getCChar $ head objects) == (getCChar $ last objects)
	                              then True
	                              else False
	                      else True
	| otherwise = False                              
   where n = length objects  

{-
    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}
instance ProblemState Level Position where
    successors (FullLevel {mapping = m}) = zip (allSquares $ FullLevel m) 
                                               (map (\pos -> move pos (FullLevel m)) (allSquares $ FullLevel m)) 
    isGoal (FullLevel {mapping = m}) = all (== True) $ map isGoalObject $ M.elems m 
    	--foldl (\acc )

    -- Doar petru BONUS
    -- heuristic =
