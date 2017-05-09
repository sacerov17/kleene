module Kleene where

import DFA
import RegExp
import qualified Data.Map as Map
import qualified Data.Set as Set

_0 :: RegExp
_0 = Symbol '0'

_1 :: RegExp
_1 = Symbol '1'

dfa :: DFA
dfa = DFA
   { states = Set.fromList [1, 2]
   , symbols = Set.fromList ['0', '1']
   , delta = Map.fromList
           [ ((1, '1'), 1)
           , ((1, '0'), 2)
           , ((2, '1'), 2)
           , ((2, '0'), 2) ]
   , start = 1
   , accepting = Set.fromList [2] }

kleene' i j 0 dfa | i == j =
  let
    transitions = -- extrae las transiciones de la funcion delta del  dfa cuando  i = j
        filter (\((i', _), j') -> i == i' && j == i') (Map.toList (delta dfa))
    characters = --  extrae los caracteres de las transiciones de la funcion delta del dfa
        map (\((_, transitionChar), _) -> transitionChar) transitions -- Mapea la funcion que nos dice el transition char con la lista que extrajimos anteriormente 
    symbols = map Symbol characters -- los caracteres de transicion los convierte en simbolos
  in
    case symbols of
      [] -> -- Caso base cuando no hay transicion cn algun caracter del estado i 
        Epsilon

      symbols ->
        foldr Plus Epsilon symbols --  Crea la expresion regular asociada a kleene i i 0 dfa 

kleene' i j 0 dfa =
  let
    transitions = -- extrae las transiciones de la funcion delta del  dfa cuando  i \= j
      filter ( \((i', _), j') -> i == i' && j == j' )  (Map.toList (delta dfa)) 
    characters = 
      map ( \((_, transitionChar), _) -> transitionChar) transitions
    symbols = map Symbol characters
  in
    case symbols of
      [] ->
        Empty

      [symbol] ->
        symbol

      (symbol:symbols) ->
        foldr Plus symbol symbols



kleene i j k dfa =
  (Plus
    (kleene' i j (k-1) dfa)
    (Dot
      (Dot
        (kleene' i k (k-1) dfa)
        (Star (kleene' k k (k-1) dfa))
      )
      (kleene' k j (k-1) dfa)
    )
  )


