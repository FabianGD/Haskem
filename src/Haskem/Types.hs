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
      displacement,
      CartesianCoord(..),
      xCoord,
      yCoord,
      zCoord
    ) where

import              Data.Char
import              Data.List
import              Lens.Micro.Platform


data CartesianCoord = CartesianCoord
        {
            _xCoord     :: Double,
            _yCoord     :: Double,
            _zCoord     :: Double
        } deriving (Eq, Show)
makeLenses ''CartesianCoord


data Atom = Atom 
    {
        _centerNumber   :: Maybe Int,
        _atomNumber     :: Int,
        _coordinate     :: CartesianCoord
    } 
    deriving (Eq, Show)
makeLenses ''Atom


data NormalMode = NormalMode
    {
        _frequency      ::  Double,
        _idx            ::  Maybe Int,
        _redMass        ::  Maybe Double,
        _forceConst     ::  Maybe Double,
        _irIntensity    ::  Maybe Double,
        _displacement   ::  [Atom]
    } deriving (Eq, Show)
makeLenses ''NormalMode


data Molecule = Molecule
    {
        _molName    :: String,
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
