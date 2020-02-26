module Stack where

import Prelude
import Data.List

type Prog = [Cmd]

data Cmd = PushN Int
         | PushB Bool
         | PushS String
         | Add
         | Sub
         | Mul
         | Div
         | Equ
         | Gt
         | Lt
    deriving (Eq,Show)

data Block
        = I Int
        | B Bool
        | S String
    deriving (Eq,Show)

type Stack = [Block]

-- I think when we do static typing we won't need the maybe
cmd :: Cmd -> Stack -> Maybe Stack
cmd (PushN i) s     = Just (I i : s)
cmd (PushB b) s     = Just (B b : s)
cmd (PushS c) s     = Just (S c : s)
cmd Add       s     = case s of
                        (I a : I b : s') -> Just (I (a+b) : s')
                        _                -> Nothing
cmd Sub       s     = case s of
                        (I a : I b : s') -> Just (I (a-b) : s')
                        _                -> Nothing
cmd Mul       s     = case s of
                        (I a : I b : s') -> Just (I (a*b) : s')
                        _                -> Nothing
cmd Div       s     = case s of
                        (_ : I 0 : s') -> Nothing
                        (I c : I d : s') -> Just (I (c `div` d) : s')
                        _                -> Nothing
cmd Equ       s     = case s of
                        (I a : I b : s') -> Just (B (a == b) : s')
                        (B c : B d : s') -> Just (B (c == d) : s')
                        (S e : S f : s') -> Just (B (e == f) : s')
                        _                -> Nothing
cmd Gt        s     = case s of
                        (I a : I b : s') -> Just (B (a>b) : s')
                        _                -> Nothing
cmd Lt        s     = case s of
                        (I a : I b : s') -> Just (B (a<b) : s')
                        _                -> Nothing

prog :: Prog -> Stack -> Maybe Stack
prog [] s       = Just s
prog (c : cs) s    = case cmd c s of
                    Just s' -> prog cs s'
                    _       -> Nothing
                     









