{-# LANGUAGE TemplateHaskell #-}

module Haskem.Types
    ( GaussianInfo(..),
      method,
      scfEnergy,
      molecule,
      Atom(..),
      atomNumber,
      coordinate,
      Molecule(..),
      molName,
      nrOfAtoms,
      atoms,
      NormalMode(..),
      frequency,
      redMass,
      forceConst,
      irIntensity,
      displacement
    ) where

import              Data.Char
import              Data.List
import              Lens.Micro.Platform


data Atom = Atom 
    {
        _centerNumber   :: Maybe Int,
        _atomNumber     :: Int,
        _coordinate     :: (Double, Double, Double)
    } 
    deriving (Eq, Show)
makeLenses ''Atom


data NormalMode = NormalMode
    {
        _frequency      ::  Double,
        _redMass        ::  Maybe Double,
        _forceConst     ::  Maybe Double,
        _irIntensity    ::  Maybe Double,
        _displacement   ::  Maybe [Atom]
    } deriving (Eq, Show)
makeLenses ''NormalMode


data Molecule = Molecule
    {
        _molName    :: Maybe String,
        _nrOfAtoms  :: Int,
        _atoms      :: [Atom]
    }   
    deriving (Eq, Show)
makeLenses ''Molecule


data GaussianInfo = GaussianInfo
    {
        _method     :: String,
        _scfEnergy  :: Double,
        _molecule   :: Maybe Molecule
    } 
    deriving (Show)
makeLenses ''GaussianInfo
