module Stack where

import Prelude
import Data.List

type Prog = [Cmd]
type Stack = [Block]
type TypeStack = [Type]
type Macro = String
type Dict = [(String, Prog)]

-- type for the return from the main language call
data Result = Ok Stack | Error
    deriving (Eq, Show)

-- type for all the language commands
data Cmd = PushB Block
         | SOp StCmd
         | BOp BoolCmd
         | MOp ArCmd
         | COp CpCmd
         | StrOp StrCmd 
         | IfElse Prog Prog
         | Define Macro Prog
         | Call Macro
         | While CpCmd Block Prog
    deriving (Eq,Show)

-- types for items that can be placed on the stack
data Block
        = I Int
        | B Bool
        | S String
    deriving (Eq,Show)

-- types that are placed on the type-stack during the staic type checking
data Type = TInt
          | TBool
          | TString
    deriving (Eq, Show)

-- stack commands
data StCmd
        = Drop
        | Dup
        | Swap
        | Over
        | Rot
    deriving (Eq,Show)

-- boolean commands
data BoolCmd
        = And
        | Or
        | Not
    deriving (Eq, Show)

-- arithmetic commands
data ArCmd
        = Add
        | Sub
        | Mul
        | Div
        | Mod
    deriving (Eq,Show)

-- comparision commands
data CpCmd
        = Equ
        | Gt
        | Lt
    deriving (Eq,Show)

data StrCmd
    = Concat
    | Slice Int Int
    deriving (Eq, Show)

-- Semantic Function
-- semantic domain = Stack -> Dict -> (Stack, Dict)
cmd :: Cmd -> Stack -> Dict -> (Stack, Dict)
cmd (PushB b) s  d  = ((b : s), d)
cmd (SOp c)   s  d  = sOp c s d
cmd (MOp c)   s  d  = mOp c s d
cmd (COp c)   s  d  = cOp c s d
cmd (BOp c) s d     = bOp c s d
cmd (StrOp c) s d   = strOp c s d
cmd (IfElse t e) s d = case s of
                        (B True : s')    -> prog t s' d
                        (B False : s')   -> prog e s' d
cmd (While c b p) s  d   = while c p b s d
cmd (Define n p) s d = (s, ((n, p) : d))
cmd (Call n)  s  d  = case lookup n d of
                        Just p -> prog p s d
                        Nothing -> (s, d)

-- Stack Operations helper function
sOp :: StCmd -> Stack -> Dict -> (Stack, Dict)
sOp c s d           = case c of
                        Drop -> case s of
                                    (a : s')         -> (s', d)
                        Dup  -> case s of
                                    (a : s')         -> ((a : a : s'), d)
                        Swap -> case s of
                                    (a : b : s')     -> ((b : a : s'), d)
                        Over -> case s of
                                    (a : b : s')     -> ((b : a : b : s'), d)
                        Rot  -> case s of
                                    (a : b : c : s') -> ((b : c : a : s'), d)

-- Math Operations helper function
mOp :: ArCmd -> Stack -> Dict -> (Stack, Dict)
mOp c s d           =  case c of
                        Add ->  case s of
                                    (I a : I b : s') -> ((I (a+b) : s'), d)
                        Sub ->  case s of
                                    (I a : I b : s') -> ((I (b-a) : s'), d)
                        Mul ->  case s of
                                    (I a : I b : s') -> ((I (a*b) : s'), d)
                        Div ->  case s of
                                    (I c : I g : s') -> ((I (g `div` c) : s'), d)
                        Mod ->  case s of
                                    (I a : I b : s') -> ((I (b `mod` a) : s'), d)

-- Compare Operations helper function
cOp :: CpCmd -> Stack -> Dict -> (Stack, Dict)
cOp c s d           =  case c of
                        Equ ->  case s of
                                    (I a : I b : s') -> ((B (a == b) : s'), d)
                                    (B c : B g : s') -> ((B (c == g) : s'), d)
                                    (S e : S f : s') -> ((B (e == f) : s'), d)
                        Gt  ->  case s of
                                    (I a : I b : s') -> ((B (a>b) : s'), d)
                        Lt  ->  case s of
                                    (I a : I b : s') -> ((B (a<b) : s'), d)

-- Boolean Operations helper function
bOp :: BoolCmd -> Stack -> Dict -> (Stack, Dict)
bOp c s d           = case c of 
                        And -> case s of 
                           (B True: B True:s') -> ((B True: s'), d)
                           (_:_: s')            -> ((B False: s'), d)
                        Or  -> case s of 
                           (B True: B True: s')  -> ((B True: s'), d)
                           (B True: B False: s') -> ((B True: s'), d)
                           (B False: B True: s') -> ((B True: s'), d)
                           (_: _: s')            -> ((B False: s'), d)
                        Not -> case s of 
                           (B True:s')  -> ((B False:s'), d)
                           (B False:s') -> ((B True:s'), d)

-- String operations helper function
strOp :: StrCmd -> Stack -> Dict -> (Stack, Dict)
strOp Concat    s  d  = case s of
                        (S a : S b : s') -> ((S (b++a) : s'), d)
strOp (Slice f t) s  d  = case s of
                          (S a : s') -> ((S (slice f t a) : s'), d)

-- slice helper
slice :: Int -> Int -> [a] -> [a]
slice f t ss = take (t - f + 1) (drop f ss)

-- while helper function
while :: CpCmd -> Prog -> Block -> Stack -> Dict -> (Stack, Dict)
while c p b s d = case ((cmd (SOp Dup) s d), (cmd (SOp Dup) [b] d)) of
                    ((s1, _), (s2, _)) -> case (cmd (COp c) ((head s1) : s2) d) of
                                              ((B False : b : cs'), d') -> (s, d')
                                              ((B True  : b : cs'), d') -> case (prog p s d') of
                                                (ds', d'') -> while c p b ds' d''

-- Semantic function for Program
-- senamtic domain = Stack -> Dict -> (Stack, Dict)
prog :: Prog -> Stack -> Dict -> (Stack, Dict)
prog [] s d         = (s, d)
prog (c : cs) s d   = case cmd c s d of
                       (s', d') -> prog cs s' d'

-- static type check stack operations
typeOfSOp :: StCmd -> TypeStack -> Maybe TypeStack
typeOfSOp Drop ts = case ts of
                      (x:xs) -> Just xs
                      _ -> Nothing
typeOfSOp Dup ts = case ts of
                     (x:xs) -> Just (x:x:xs)
                     _ -> Nothing
typeOfSOp Swap ts = case ts of
                     (x:y:xs) -> Just (y:x:xs)
                     _ -> Nothing
typeOfSOp Over ts = case ts of
                     (x:y:xs) -> Just (y:x:y:xs)
                     _ -> Nothing
typeOfSOp Rot ts = case ts of
                     (x:y:z:xs) -> Just (y:z:x:xs)
                     _ -> Nothing

-- static type check comparison operations
typeOfCOp :: CpCmd -> TypeStack -> Maybe TypeStack
typeOfCOp Equ ts = case ts of 
                    (TInt:TInt:xs) -> Just (TBool:xs)
                    (TBool:TBool:xs) -> Just (TBool:xs)
                    (TString:TString:xs) -> Just (TBool:xs)
                    _ -> Nothing
typeOfCOp _ ts = case ts of
                   (TInt:TInt:xs) -> Just (TBool:xs)
                   _ -> Nothing

-- static type check boolean operations
typeOfBOp :: BoolCmd -> TypeStack -> Maybe TypeStack
typeOfBOp Not ts = case ts of
                    (TBool:xs) -> Just (TBool:xs)
                    _ -> Nothing
typeOfBOp _ ts   = case ts of
                    (TBool:TBool:xs) -> Just (TBool:xs)
                    _ -> Nothing

-- static type check string operations
typeOfStrOp :: StrCmd -> TypeStack -> Maybe TypeStack
typeOfStrOp Concat ts = case ts of
                        (TString : TString : ts') -> Just (TString : ts')
                        _ -> Nothing
typeOfStrOp (Slice f t) ts = case ts of
                          (TString : ts') -> Just (TString : ts')
                          _ -> Nothing

-- static type check a command
typeOf :: Cmd -> TypeStack -> Dict -> Maybe (TypeStack, Dict)
typeOf (PushB b) ts d = case b of 
                          (I x) -> Just ((TInt:ts), d)
                          (B y) -> Just ((TBool:ts), d)
                          (S z) -> Just ((TString:ts), d)
typeOf (SOp c) ts d = case (typeOfSOp c ts) of
                          Just ts' -> Just (ts', d)
                          _ -> Nothing
typeOf (MOp c) ts d = case ts of
                          (TInt:TInt:ts') -> Just ((TInt:ts'), d)
                          _ -> Nothing
typeOf (COp c) ts d = case (typeOfCOp c ts) of
                          Just ts' -> Just (ts', d)
                          _ -> Nothing
typeOf (StrOp c) ts d = case (typeOfStrOp c ts) of
                          Just ts' -> Just (ts', d)
                          _ -> Nothing
typeOf (BOp c) ts d = case (typeOfBOp c ts) of 
                          Just ts' -> Just (ts', d)
                          _ -> Nothing
typeOf (IfElse t e) ts d = case ts of
                            (TBool:ts') -> Just (ts', d)
                            _ -> Nothing       
typeOf (Define n p) ts d = Just (ts, ((n,p):d))
typeOf (Call n) ts d     = case lookup n d of
                            Just p -> case (typeCheck p ts d) of
                                       Just (ts', d') -> Just (ts', d')
                                       _              -> Nothing
                            _ -> Nothing
typeOf (While c b p) ts d = case ts of
                            (x:xs) -> Just ((x:xs), d)
                            _ -> Nothing


-- This function performs the static type checking for a program
-- Returns Just (TypeStack, Dict) if the program has no type errors
-- Returns Nothing if there is a type error
-- semantic domain = TypeStack -> Dict -> Maybe (TypeStack, Dict)
typeCheck :: Prog -> TypeStack -> Dict -> Maybe (TypeStack, Dict)
typeCheck [] ts d = Just (ts, d)
typeCheck ((IfElse t e):cs) ts d = case (typeOf (IfElse t e) ts d) of
                                        Just (ts', d) -> case ((typeCheck (t++cs) ts' d), (typeCheck (e++cs) ts' d)) of
                                             (Just _, Just _) -> Just (ts', d)
                                             _                -> Nothing
                                        _ -> Nothing
typeCheck (c:cs) ts d = case (typeOf c ts d) of
                             Just (ts', d') -> typeCheck cs ts' d'
                             _              -> Nothing 

-- Run a program
-- Returns Ok Stack if the program passes they type checking
-- Returns Error if there was a type error
stackm :: [Cmd] -> Result
stackm [] = Ok []
stackm p = case (typeCheck p [] []) of
             Just _ -> case (prog p [] []) of
                         (s, d) -> Ok s
             Nothing -> Error

-- macro for >=
-- usage example: stackm [PushB (I 3), PushB (I 4), makeLTE, callLTE]
-- output: Ok [B True]
makeGTE :: Cmd
makeGTE = Define "gte" [SOp Dup, SOp Rot, SOp Over, COp Gt, SOp Rot, COp Equ, BOp Or]

callGTE :: Cmd 
callGTE = Call "gte"

-- macro for <=
-- usage example: stackm [PushB (I 3), PushB (I 4), makeLTE, callLTE]
-- output: Ok [B True]
makeLTE :: Cmd
makeLTE = Define "lte" [SOp Dup, SOp Rot, SOp Over, COp Lt, SOp Rot, COp Equ, BOp Or]

callLTE :: Cmd 
callLTE = Call "lte"

-- gcd example program
gcdExample = [PushB (I 210), PushB (I 45), SOp Over, SOp Over, COp Gt, IfElse [SOp Swap] [], While Gt (I 0) [SOp Dup, SOp Rot, MOp Mod], SOp Drop]

-- sum of numbers 0-10
sumExample = [PushB (I 0), PushB (I 0), While Lt (I 10) [PushB (I 1), MOp Add, SOp Dup, SOp Rot, MOp Add, SOp Swap], SOp Drop]

-- bad example for underflow
underflowExample = [PushB (I 0), MOp Add]

-- bad example for wrong types for an operation
typesExample = [PushB (I 0), PushB (S "Hello world!"), MOp Add]

-- bad example for while loop that never ends
-- this will loop forever
badWhileExample = [PushB (I 0), While Gt (I (-1)) [PushB (I 1), MOp Add]]

-- bad example of concatenation
concatExample = [PushB (S "Hello World!"), PushB (S "Another String"), StrOp Concat, StrOp Concat]
