module Stack where

import Prelude
import Data.List

type Prog = [Cmd]

data Cmd = PushN Int
         | PushB Bool
         | PushS String
         | Add
         | Mul
         | Equ
    deriving (Eq,Show)

data Stack
        = I Int
        = B Bool
        = S Stack
    deriving (Eq,Show)
