module Fuzzy exposing(..)

import Char exposing (fromCode)
import String exposing (fromChar)
import List exposing (map2, foldr)

-- ref: http://www.sist.ac.jp/~kanakubo/research/reasoning_kr/fuzzy.html

get_color : Float -> Bool -> String
get_color inp p =
  let
    getChr c = if c < 10 then toString c else fromChar <| fromCode (87+c)
    getStr n b = if n < b then getChr n else (toRadix16 (n//b)) ++  (getChr (n%b))
    toRadix16 n = getStr n 16
  in
    inp |> abs |> round |> toRadix16
    |> (\str -> case String.length str of
                0 -> "00"
                1 -> "0" ++ str
                _ -> str)
    |> String.repeat 2 |> (if p then (++) "ff" else \x -> x ++ "ff") |> (++) "#"

mem_hot : Float -> Float
mem_hot temp = if temp < 10 then 0.0 else if temp > 30 then 1.0 else temp / 20.0 - 0.5

mem_cold : Float -> Float
mem_cold temp = if temp < 10 then 1.0 else if temp > 30 then 0.0 else 1.5 - temp / 20.0

inference : Float -> Float -> Float
inference i o =
  let
    rule =
      [ (mem_hot i) * (mem_hot o)
      , (mem_hot i) * (mem_cold o)
      , (mem_cold i) * (mem_hot o)
      , (mem_cold i) * (mem_cold o)
      ]

    f = [-2.0, 0.2, -0.2, 2.0]
  in
    (map2 (*) rule f |> foldr (+) 0) / foldr (+) 0 rule
