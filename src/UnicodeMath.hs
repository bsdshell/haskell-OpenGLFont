
-- | Some unicode math symbols for testing purposes


module UnicodeMath where

--------------------------------------------------------------------------------


import qualified Data.Map as Map
import Data.Map (Map)

--------------------------------------------------------------------------------

-- * for testing


greeks = 
  [ alpha, beta, gamma, delta, epsilon, zeta, eta, theta
  , iota, kappa, lambda, mu, nu, xi {- ,omicron -} 
  , pi_, rho, sigma, tau, upsilon, phi, chi, psi, omega 
  ]

math_test =
  [ forall_ , alpha, beta, gamma , '.' , ' ' 
  , alpha , ostar , '(' , beta , oplus , gamma , ')' , ' ' , '=' 
  , alpha , ostar, beta , oplus , alpha, ostar , gamma
  ]

--------------------------------------------------------------------------------


latexToChar :: String -> Maybe Char
latexToChar s = Map.lookup s latexSymbolTable

latexSymbolTable :: Map String Char
latexSymbolTable = Map.fromList 
  [ 
    "forall" ~> forall_
  , "exists" ~> exists
  , "prod"   ~> prod
  , "coprod" ~> coprod
  , "sum"    ~> sum_
    --

  , "oplus"  ~> oplus
  , "ominus" ~> ominus
  , "otimes" ~> otimes
  , "odot"   ~> odot
  , "ostar"  ~> ostar
    -- greek

  , "alpha"   ~> alpha
  , "beta"    ~> beta
  , "gamma"   ~> gamma
  , "delta"   ~> delta
  , "epsilon" ~> epsilon
  , "zeta"    ~> zeta
  , "eta"     ~> eta
  , "theta"   ~> theta
    -- std blackboard numbers

  , "Z" ~> zz
  , "N" ~> nn
  , "Q" ~> qq
  , "R" ~> rr
  , "C" ~> cc
    -- some letterlike

  , "ell"   ~> ell
  , "hbar"  ~> hbar
  , "aleph" ~> aleph
    -- brackets

  , "langle" ~> langle
  , "rangle" ~> rangle
  ]
  where
    (~>) a b = (a,b)

--------------------------------------------------------------------------------

-- * misc other


openbox = '\x2423'   -- blank 


--------------------------------------------------------------------------------

-- * brackets


langle = '\x2329'
rangle = '\x232a'

--------------------------------------------------------------------------------

-- * ligatures


ff  = '\xfb00'
fi  = '\xfb01'
fl  = '\xfb02'
ffi = '\xfb03'
ffl = '\xfb04'

--------------------------------------------------------------------------------

-- * some math operators


forall_ = '\x2200'
exists  = '\x2203'

prod   = '\x220f'
coprod = '\x2210'
sum_   = '\x2211'

infty  = '\x221e'
rightarrow = '\x2192'

oplus  = '\x2295'
ominus = '\x2296'
otimes = '\x2297'
odot   = '\x2299'
ostar  = '\x229b'

--------------------------------------------------------------------------------

-- * some letterlike


hbar  = '\x210f'
ell   = '\x2113'
aleph = '\x2135'

aa = '\x1d538'
pp = '\x2119'

cc = '\x2102'
nn = '\x2115'
zz = '\x2124'
qq = '\x211a'
rr = '\x211d'

--------------------------------------------------------------------------------

-- * greek lowercase


alpha   = '\x03b1'
beta    = '\x03b2'
gamma   = '\x03b3'
delta   = '\x03b4'
epsilon = '\x03b5'
zeta    = '\x03b6'
eta     = '\x03b7'
theta   = '\x03b8'
iota    = '\x03b9'
kappa   = '\x03ba'
lambda  = '\x03bb'
mu      = '\x03bc'
nu      = '\x03bd'
xi      = '\x03be'
omicron = '\x03bf'
pi_     = '\x03c0'
rho     = '\x03c1'
sigma   = '\x03c3'
tau     = '\x03c4'
upsilon = '\x03c5'
phi     = '\x03c6'
chi     = '\x03c7'
psi     = '\x03c8'
omega   = '\x03c9'

--------------------------------------------------------------------------------

-- * some cyrillic


lje   = '\x0409'
de    = '\x0414'
zhe   = '\x0416'
sha   = '\x0428' 
shcha = '\x0429' 

--------------------------------------------------------------------------------
