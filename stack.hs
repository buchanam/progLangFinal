module Stack where

import Prelude
import Data.List

type Prog = [Cmd]

data Cmd = PushN Int
         | PushB Bool
         | PushS String
         | Pop
         | Add
         | Sub
         | Mul
         | Div
         | Equ
         | Gt
         | Lt
         | Concat
    deriving (Eq,Show)

data Block
        = I Int
        | B Bool
        | S String
    deriving (Eq,Show)

type Stack = [Block]

-- I think when we do static typing we won't need the maybe
cmd :: Cmd -> Stack -> Stack
cmd (PushN i) s     = I i : s
cmd (PushB b) s     = B b : s
cmd (PushS c) s     = S c : s
cmd Pop       s     = case s of
                        (a : s')         -> s'
cmd Add       s     = case s of
                        (I a : I b : s') -> (I (a+b) : s')
cmd Sub       s     = case s of
                        (I a : I b : s') -> (I (a-b) : s')
cmd Mul       s     = case s of
                        (I a : I b : s') -> (I (a*b) : s')
cmd Div       s     = case s of
                        (I c : I d : s') -> (I (c `div` d) : s')
cmd Equ       s     = case s of
                        (I a : I b : s') -> (B (a == b) : s')
                        (B c : B d : s') -> (B (c == d) : s')
                        (S e : S f : s') -> (B (e == f) : s')
cmd Gt        s     = case s of
                        (I a : I b : s') -> (B (a>b) : s')
cmd Lt        s     = case s of
                        (I a : I b : s') -> (B (a<b) : s')
cmd Concat    s     = case s of
                        (S a : S b : s') -> (S (a++b) : s')

prog :: Prog -> Stack -> Stack
prog [] s       = s
prog (c : cs) s    = case cmd c s of
                    s' -> prog cs s'
                     









