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
			          
--Scrub data 
elimEmpty :: [[[String]]] -> [[[String]]]
elimEmpty [] = []
elimEmpty (x:xs) = (map elimEmpty' x) : (elimEmpty xs)
 where  
   elimEmpty' :: [String] -> [String]
   elimEmpty' [] = []
   elimEmpty' ("":xs) = elimEmpty' xs
   elimEmpty' (x:xs) = x : elimEmpty' xs

   
-- elimEmpty [] = [] --error "Um...NOPE!!!"
-- elimEmpty ([]:xs) = xs
-- elimEmpty ([[x]]:xs) = [[x]] : elimEmpty xs
-- elimEmpty (([x'] : xs') : xs) = ([x'] : xs') : elimEmpty xs
-- elimEmpty (((x'' : xs'') : xs') : xs) | x'' == "" = elimEmpty ((xs'':xs'):xs)  						   --first element is empty
-- 				      | last xs'' == "" = ((x'': (init xs'')) : xs') : elimEmpty xs  --last element is empty
--                                       | last xs'  == "" = ((x'' : xs'') : (init xs')) : elimEmpty xs
-- 				      | otherwise = (((x'' : xs'') : xs') : elimEmpty xs)
									  
									  
-----------------------------
--Get individual units of TM
-----------------------------
--States:
listStates :: TM -> [String]
listStates (TM states _ _ _ _ _ _) = states

--Start
stringStart :: TM -> String
stringStart (TM _ start _ _ _ _ _) = start

--Accept
stringAccept :: TM -> String
stringAccept (TM _ _ accept _ _ _ _) = accept

--Reject
stringReject :: TM -> String
stringReject (TM _ _ _ reject _ _ _) = reject

--Alpha
listAlpha :: TM -> [String]
listAlpha (TM _ _ _ _ alpha _ _) = alpha

--Tape-Alpha
listTapeAlpha :: TM -> [String]
listTapeAlpha (TM _ _ _ _ _ tapeAlpha _) = tapeAlpha 

--Transitions
listTransitions :: TM -> [((String, String), (String, String, String))]
listTransitions (TM _ _ _ _ _ _ transitions) = transitions

getTrans :: ((String, String), (String, String, String)) -> String
getTrans ((a, _), (_, _, _)) = a

getTransAlpha :: ((String, String), (String, String, String)) -> [String]
getTransAlpha ((_, _ ), (a, b, _)) = [a,b]

getTransStates :: ((String, String), (String, String, String)) -> [String]
getTransStates ((_, a), (_, _, b)) = [a,b]

-----------------------------

-----------------------------------
--Verify input validity
-----------------------------------
--verify String data types
verifyTMStrings :: String -> [String] -> Bool
verifyTMStrings start states= start `elem` states

-- verify list data types
verifyTMLists :: [String] -> [String] -> Bool
verifyTMLists alpha tapeAlpha = do
	let boolList = map (\x -> x `elem` tapeAlpha) alpha
	False `notElem` boolList

--verify transitions
verifyTrans :: [String] -> [String] -> [((String, String),(String, String, String))] -> IO ()
verifyTrans transCons states trans = do
	--accumulate transitions
	let transList = map getTrans trans
	let boolList = map (\x -> x `elem` transCons) transList
	let transAlpha = map getTransAlpha trans
	let transAlpha' = concat transAlpha
	let transStates = map getTransStates trans
	let transStates' = concat transStates
	print transAlpha
	print transAlpha'
	print transStates
	print transStates'
	--False `notElem` boolList
	--undefined
-----------------------------------

	
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
	   let fileLine = lines fileContents
	   let fileWords = map words fileLine
	   let fileItems = map (map (splitOneOf "{},:;")) fileWords
	   
	   --Delete empty strings in fileItems
	   let scrubbedData = elimEmpty fileItems
	   
	   --Parse data and put into TM record
	   let parsedData = parseLines scrubbedData
	   
	   --Get elements of TM
	   let lsStates = listStates parsedData				--get states
	   let sStart = stringStart parsedData				--get start
	   let sAccept = stringAccept parsedData			--get accept
	   let sReject = stringReject parsedData			--get reject
	   let lsAlpha = listAlpha parsedData				--get alpha
	   let lsTapeAlpha = listTapeAlpha parsedData		--get tape-alpha
	   let lsTransitions = listTransitions parsedData	--get transitions
	   
	   
	   
	   --Verify input
	   let verifStart = verifyTMStrings sStart lsStates
	   let verifAccept = verifyTMStrings sAccept lsStates
	   let verifReject = verifyTMStrings sReject lsStates
	   let verifAlpha = verifyTMLists lsAlpha lsTapeAlpha
	   
	   --Constants for transitions
	   let transConstants = ["rwRt","rwLt","rRl","rLl","rRt","rLt"]

	   verifyTrans transConstants lsStates lsTransitions
	   --print list
	   -- print lsStates
	   -- print sStart
	   -- print sAccept
	   -- print sReject
	   -- print lsAlpha
	   -- print lsTapeAlpha
	   -- print lsTransitions
	   print (verifyTMLists lsAlpha lsTapeAlpha)
	   

	   
testFile :: [[String]] -> IO ()
testFile testList2 = mapM_ (mapM_ print) testList2
		
	-- do goodData <- elimEmpty testList
	   -- show goodData
	   --[show a | z <- goodData, y <- z, a <- y]
	    
testList2 = [[1,2,3],[4,5,6]]
testList = [[["--"],["Initialization",""]], [["","states",""],["Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","A","R",""]]]                 


--
parseLines' :: [[[String]]] -> [String] -> String -> String -> String -> [String] -> [String] -> [((String, String), (String, String, String))]  -> TM
parseLines' [] states start accept reject alpha tapeAlpha transitions = TM states start accept reject alpha tapeAlpha transitions
parseLines' ([]:rest) states start accept reject alpha tapeAlpha transitions = parseLines' rest states start accept reject alpha tapeAlpha transitions                 		-- Empty line
parseLines' (([]:rest'):rest) states start accept reject alpha tapeAlpha transitions = parseLines' (rest':rest) states start accept reject alpha tapeAlpha transitions 
parseLines' (((e:es):rest'):rest) states start accept reject alpha tapeAlpha transitions | e == "--" = parseLines' (rest':rest)  states start accept reject alpha tapeAlpha transitions
                                                                                         | e == "states" = parseLines' (rest':rest) (head rest') start accept reject alpha tapeAlpha transitions
											 | e == "start" = parseLines' (rest':rest) states (head(head rest')) accept reject alpha tapeAlpha transitions
											 | e == "accept" = parseLines' (rest':rest) states start (head(head rest')) reject alpha tapeAlpha transitions
											 | e == "reject" = parseLines' (rest':rest) states start accept (head(head rest')) alpha tapeAlpha transitions
											 | e == "alpha" = parseLines' (rest':rest) states start accept reject (head rest') tapeAlpha transitions
											 | e == "tape-alpha" = parseLines' (rest':rest) states start accept reject alpha (head rest') transitions
											 | e == "rwRt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e,(head rest' !! 0)), ((tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0),((tail rest') !! 2 !! 0)))])
											 | e == "rRl" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), (tail rest' !! 0 !! 0,tail rest' !! 0 !! 0,(head rest' !! 0)))])
											 | e == "rRt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), ((tail rest' !! 0 !! 0),(tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0)))])
											 | e == "rwLt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e,(head rest' !! 0)), ((tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0),((tail rest') !! 2 !! 0)))])
											 | e == "rLl" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), (tail rest' !! 0 !! 0,tail rest' !! 0 !! 0,(head rest' !! 0)))])
											 | e == "rLt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), ((tail rest' !! 0 !! 0),(tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0)))])
											 | otherwise = parseLines' (rest':rest) states start accept reject alpha tapeAlpha transitions    --throw an error on this line???
parseLines :: [[[String]]] -> TM
parseLines f = parseLines' f [] "" "" "" [] [] [] 
               
-- parseStates :: [[[String]]] -> [String]
-- parseStates [] = []
-- parseStates [[[xs]]] = [parseStates[[xs]]]
-- parseStates[[xs]] = [parseStates[xs]]
-- parseStates[xs] = case (xs !! 1) of "states" -> xs


									
--c:\Users\Asus\Desktop\project2Test.txt
--Empty File:
--C:\Users\Asus\Desktop\NewTextDocument(2).txt













