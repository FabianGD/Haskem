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
      zCoord,
      AtomSpec(..),
      element,
      geomNum,
      LinearBond(..),
      bAtomA,
      bAtomB,
      bLengt,
      Angle(..),
      aAtomA,
      aAtomB,
      aAtomC,
      aAngle,
      Dihedral(..),
      dAtomA,
      dAtomB,
      dAtomC,
      dAtomD,
      dAngle
    ) where

import              Data.Char
import              Data.List
import              Lens.Micro.Platform
import              Data.Vector

{-
Data types to store a molecule in cartesian space
-}

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


{-
Data types for redundant internal coordinates:
-}
data AtomSpec = AtomSpec {
    _element ::  String,
    _geomNum ::  Int
    } deriving (Eq, Show)
makeLenses ''AtomSpec


data LinearBond = LinearBond {
    _bAtomA ::  AtomSpec,
    _bAtomB ::  AtomSpec,
    _bLengt ::  Double
    } 
    deriving (Eq, Show)
makeLenses ''LinearBond


data Angle = Angle {
    _aAtomA ::  AtomSpec,
    _aAtomB ::  AtomSpec,
    _aAtomC ::  AtomSpec,
    _aAngle ::  Double
    } 
    deriving (Eq, Show)
makeLenses ''Angle  


data Dihedral = Dihedral {
    _dAtomA ::  AtomSpec,
    _dAtomB ::  AtomSpec,
    _dAtomC ::  AtomSpec,
    _dAtomD ::  AtomSpec,
    _dAngle ::  Double
    }
    deriving (Eq, Show)
makeLenses ''Dihedral


data Fragment = Fragment {
    _name       ::  String,
    _bonds      ::  Vector LinearBond,
    _angles     ::  Vector Angle,
    _dihedral   ::  Vector Dihedral
}