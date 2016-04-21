{-# OPTIONS_GHC -fno-warn-tabs #-} --eliminates Warning: Tab character
--import TMAssemblyHelper as TMAH
import GHC.Generics
import System.IO
import Data.List
import Data.List.Split

data TM = TM {  states :: [String]
			  , start :: String
			  , accept :: String
			  , reject :: String
			  , alpha :: [String]
			  , tapeAlpha :: [String]
			  , transitions :: [((String, String), (String, String, String))]
			  } deriving (Show)
			  
elimEmpty :: [[[String]]] -> [[[String]]]
elimEmpty ([[x]]:xs) = [[x]] : elimEmpty xs
elimEmpty (([x'] : xs') : xs) = ([x'] : xs') : elimEmpty xs
elimEmpty (((x'' : xs'') : xs') : xs) | x'' == "" = elimEmpty ((xs'':xs'):xs)  						   --first element is empty
									  | last xs'' == "" = (((x'': (init xs'')) : xs') : elimEmpty xs)  --last element is empty
									  | otherwise = (((x'' : xs'') : xs') : xs)
									  
									  

	
listStates :: TM -> [String]
listStates (TM states _ _ _ _ _ _) = states
	
main = do
--Get file path and word to test
	putStrLn("Enter the absolute path of your Turing Machine configuration.")
	configPath <- getLine
	putStrLn("What word would you like to test on the Turing Machine?")
	testWord <- getLine
	
--Get file contents
	fileHandle <- openFile configPath ReadMode
	parseFile fileHandle
	
--Close handle when done parsing
	hClose fileHandle

--TODO: Find a way to check if file is empty and return error
-- to user
parseFile :: Handle -> IO ()
parseFile fileHandle = 
	do fileContents <- hGetContents fileHandle
	   let line = lines fileContents
	   let elem = map words line
	   let items = map (map (splitOneOf "{},:;")) elem
	   
	   --delete empty strings in items
	   let goodData = elimEmpty items
	   
	   print goodData
	   
	   let lsStates = parseLines testList
	   
	   --print list
	   print lsStates
	   
	   --ucket (Glossary title _)) -> putStrLn title

	   
testFile :: [[String]] -> IO ()
testFile testList2 = mapM_ (mapM_ print) testList2
		
	-- do goodData <- elimEmpty testList
	   -- show goodData
	   --[show a | z <- goodData, y <- z, a <- y]
	    
testList2 = [[1,2,3],[4,5,6]]
testList = [[["--"],["Initialization",""]], [["","states",""],["Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","A","R",""]]]                 

parseLines' :: [[[String]]] -> [String] -> String -> String -> String -> [String] -> [String] -> [((String, String), (String, String, String))]  -> TM
parseLines' [] states start accept reject alpha tapeAlpha transitions = TM states start accept reject alpha tapeAlpha transitions
parseLines' ([]:rest) states start accept reject alpha tapeAlpha transitions = parseLines' rest states start accept reject alpha tapeAlpha transitions                 -- Empty line
parseLines' (([]:rest'):rest) states start accept reject alpha tapeAlpha transitions = parseLines' (rest':rest) states start accept reject alpha tapeAlpha transitions -- ???
parseLines' (((e:es):rest'):rest) states start accept reject alpha tapeAlpha transitions | e == "--" = parseLines' (rest':rest)  states start accept reject alpha tapeAlpha transitions
                                                                                         | e == "states" = parseLines' (rest':rest) es start accept reject alpha tapeAlpha transitions
																						 | e == "start" = parseLines' (rest':rest) states (head es) accept reject alpha tapeAlpha transitions
                                                                                         | otherwise = parseLines' (rest':rest) states start accept reject alpha tapeAlpha transitions
parseLines :: [[[String]]] -> TM
parseLines f = parseLines' f [] "" "" "" [] [] [(("",""),("","",""))] 
                 
-- parseStates :: [[[String]]] -> [String]
-- parseStates [] = []
-- parseStates [[[xs]]] = [parseStates[[xs]]]
-- parseStates[[xs]] = [parseStates[xs]]
-- parseStates[xs] = case (xs !! 1) of "states" -> xs


									
--c:\Users\Asus\Desktop\project2Test.txt
--Empty File:
--C:\Users\Asus\Desktop\NewTextDocument(2).txt

