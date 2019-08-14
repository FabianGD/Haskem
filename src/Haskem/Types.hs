{-# LANGUAGE TemplateHaskell #-}

module Haskem.Types
    ( GaussianInfo(..),
      method,
      scfEnergy,
      Atom(..),
      atomNumber,
      coordinate,
      Molecule(..),
      molName,
      atoms
    ) where

import              Data.Char
import              Data.List
import              Lens.Micro.Platform


data GaussianInfo = GaussianInfo
    {
        _method     :: String,
        _scfEnergy  :: Double
    } 
    deriving (Show)
makeLenses ''GaussianInfo


data Atom = Atom 
    {
        _atomNumber :: Int,
        _coordinate :: (Double, Double, Double)
    } 
    deriving (Eq, Show)
makeLenses ''Atom


data Molecule = Molecule
    {
        _molName    :: Maybe String,
        _atoms      :: [Atom]
    }   
    deriving (Eq, Show)
makeLenses ''Molecule