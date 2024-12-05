{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bank where

import Control.Monad.State

-- The BankOp type is a State monad that operates on a Float (the balance) and returns a value of type a
newtype BankOp a = BankOp { runBankOp' :: State Float a }
    deriving (Functor, Applicative, Monad)  -- Derive Functor, Applicative, and Monad

-- Deposit operation: Adds the specified amount to the balance
deposit :: Float -> BankOp ()
deposit amount = BankOp $ modify (+ amount)

-- Withdraw operation: Withdraws the specified amount, but allows overdraft up to $100
withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ do
    balance <- get
    let available = balance + 100.0  -- Allow up to $100 overdraft
    let withdrawn = min amount available  -- Withdraw as much as possible
    put (balance - withdrawn)  -- Update the balance after withdrawal
    return withdrawn  -- Return the actual amount withdrawn

-- Get the current balance
getBalance :: BankOp Float
getBalance = BankOp $ get

-- Function to run a BankOp starting with an initial balance of 0.0
runBankOp :: BankOp a -> a
runBankOp bankOp = evalState (runBankOp' bankOp) 0.0
