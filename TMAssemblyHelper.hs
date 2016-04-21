{-# OPTIONS_GHC -fno-warn-tabs #-}
module TMAssemblyHelper
( states
, start
, accept
, reject
, alpha
, tapeAlpha
, transitions
, TM
--, parser
) where

data TM = TM {  states :: [String]
			  , start :: String
			  , accept :: String
			  , reject :: String
			  , alpha :: [String]
			  , tapeAlpha :: [String]
			  , transitions :: [((String, String), (String, String, String))]
			  } deriving (Show)


