module Transformers where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map


type Name = String -- variable names

data Exp = Lit Integer -- expressions
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
    deriving (Show)

data Value = IntVal Integer -- values
           | FunVal Env Name Exp
    deriving (Show)

type Env = Map.Map Name Value -- mapping from names to values


-- simple style
eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of
                            FunVal env0 n body -> eval0 (Map.insert n val2 env0) body


-- monadic style (to prepare for monad transformer use)
-- here we use the Identity monad as a wrapper for the result
type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust (Map.lookup n env)
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                               FunVal env0 n body -> eval1 (Map.insert n val2 env0) body


-- with error handling
type Eval2 a = ErrorT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runErrorT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = return $ fromJust (Map.lookup n env)
eval2a env (Plus e1 e2) = do IntVal i1 <- eval2a env e1
                             IntVal i2 <- eval2a env e2
                             return $ IntVal (i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            case val1 of
                                FunVal env0 n body -> eval2a (Map.insert n val2 env0) body

-- with error messages
eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = case (Map.lookup n env) of
                         Just a  -> return a
                         Nothing -> throwError "Key not found"
eval2b env (Plus e1 e2) = do e1' <- eval2b env e1
                             e2' <- eval2b env e2
                             case (e1', e2') of
                                 (IntVal i1 ,IntVal i2) -> return $ IntVal (i1 + i2)
                                 _                      -> throwError "type error"
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do val1 <- eval2b env e1
                            val2 <- eval2b env e2
                            case val1 of
                                FunVal env0 n body -> eval2b (Map.insert n val2 env0) body
                                _                  -> throwError "type error"


-- use the fcat that do uses fail in case of error
eval2c :: Env -> Exp -> Eval2 Value
eval2c env (Lit i) = return $ IntVal i
eval2c env (Var n) = case (Map.lookup n env) of
                         Just a  -> return a
                         Nothing -> throwError "Key not found"
eval2c env (Plus e1 e2) = do IntVal i1 <- eval2c env e1
                             IntVal i2 <- eval2c env e2
                             return $ IntVal (i1 + i2)
eval2c env (Abs n e) = return $ FunVal env n e
eval2c env (App e1 e2) = do FunVal env0 n body <- eval2c env e1
                            val2               <- eval2c env e2
                            eval2c (Map.insert n val2 env0) body

-- final error version
eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
                        Nothing  -> throwError ("unbound variable: " ++ n)
                        Just val -> return val
eval2 env (Plus e1 e2) = do e1' <- eval2 env e1
                            e2' <- eval2 env e2
                            case (e1', e2') of
                                (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                _                      -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do val1 <- eval2 env e1
                           val2 <- eval2 env e2
                           case val1 of
                               FunVal env0 n body -> eval2 (Map.insert n val2 env0) body
                               _                  -> throwError "type error in application"


-- with Reader monad
type Eval3 a = ReaderT Env (ErrorT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runErrorT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                       Nothing  -> throwError ("unbound variable: " ++ n)
                       Just val -> return val
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _                      -> throwError "type error in addition"
eval3 (Abs n e) = do env <- ask
                     return $ FunVal env n e
eval3 (App e1 e2 ) = do val1 <- eval3 e1
                        val2 <- eval3 e2
                        case val1 of
                            FunVal env0 n body -> local (const (Map.insert n val2 env0)) (eval3 body)
                            _                  -> throwError "type error in application"


-- with state
type Eval4 a = ReaderT Env (ErrorT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do tick
                   return $ IntVal i
eval4 (Var n) = do tick
                   env <- ask
                   case Map.lookup n env of
                       Nothing  -> throwError ("unbound variable: " ++ n)
                       Just val -> return val
eval4 (Plus e1 e2) = do tick
                        e1' <- eval4 e1
                        e2' <- eval4 e2
                        case (e1', e2') of
                            (IntVal i1 ,IntVal i2) -> return $ IntVal (i1 + i2)
                            _                      -> throwError "type error in addition"
eval4 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval4 (App e1 e2) = do tick
                       val1 <- eval4 e1
                       val2 <- eval4 e2
                       case val1 of
                           FunVal env0 n body -> local (const (Map.insert n val2 env0)) (eval4 body)
                           _                  ->throwError "type error in application"
