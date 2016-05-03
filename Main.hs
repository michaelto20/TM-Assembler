{-# OPTIONS_GHC -fno-warn-tabs #-} --eliminates Warning: Tab character
--import TMAssemblyHelper as TMAH
import GHC.Generics
import System.IO
import Data.List
import Data.List.Split
import Prelude hiding (catch)
import System.Directory

data TM = TM {  states :: [String]
			  , start :: String
			  , accept :: String
			  , reject :: String
			  , alpha :: [String]
			  , tapeAlpha :: [String]
			  , transitions :: [((String, String), (String, String, String))]
			  } deriving (Show)
			          
					  
-- data MyExceptions = badDataException | rejectWordException
	-- deriving(Show, Typeable)
	
-- instance Exception MyExceptions
				
--Scrub data 
elimEmpty :: [[[String]]] -> [[[String]]]
elimEmpty [] = []
elimEmpty (x:xs) = (map elimEmpty' x) : (elimEmpty xs)
 where  
   elimEmpty' :: [String] -> [String]
   elimEmpty' [] = []
   elimEmpty' ("":xs) = elimEmpty' xs
   elimEmpty' (x:xs) = x : elimEmpty' xs
									  
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
verifyTrans :: [String] -> [String] -> [String] -> [((String, String),(String, String, String))] -> Bool
verifyTrans transCons tapeAlphabet states trans = do

	--accumulate and verify transitions
	let transList = map getTrans trans
	let boolList = map (\x -> x `elem` transCons) transList
	let transFlag = False `notElem` boolList
	
	--accumulate and verify transition alphabet
	let transAlpha = map getTransAlpha trans
	let transAlpha' = concat transAlpha
	let transAlphaBool = map (\x -> x `elem` tapeAlphabet) transAlpha'
	let transAlphaFlag = False `notElem` transAlphaBool
	
	--accumulate and verify transition states
	let transStates = map getTransStates trans
	let transStates' = concat transStates
	let transStatesBool = map (\x -> x `elem` states) transStates'
	let transStatesFlag = False `notElem` transStatesBool
	
	transStatesFlag && transAlphaFlag && transFlag

--End Verify Input Region
----------------------------------------------------------

runError :: Bool -> IO ()
runError a = error "Slow down cowboy, you're riding your horse backwards!"

	
findTransition :: [((String, String), (String, String, String))] -> String -> String -> Int -> [Bool]
findTransition trans currentState word position = do
	let letter = getChar' position word
	map (\x -> findTransition' x currentState letter) trans
	
	
getChar' :: Int -> String -> Char
getChar' position word = do
	if(position > length word)
		then error "Err..Something went wrong, go ahead and take five!"
		else if(length word == position)
			then word !! (position -1)
			else (word !! position)


getInputState :: ((String, String), (String, String, String)) -> [(String, String, String)]
getInputState ((a,_),(_,b,c)) = [(a,b,c)]

findTransition' :: ((String, String), (String, String, String)) -> String -> Char -> Bool
findTransition' transition state letter = do
	if((getCurrentState transition) == state)
		then if((getInput' transition) == (convertToString letter))
			then True
			else False
		else False
	
findTrue :: [Bool] -> Int -> String -> Int -> String -> Int
findTrue boolList position word wordPosition currentState = do
	if(position >= length boolList)
		then error ("Rejected: " ++(getWordState word wordPosition currentState ""))
		else do
			if((boolList !! position) == True)
				then position
				else findTrue boolList (position+1) word wordPosition currentState
					
		
getTransition :: Int -> String -> Int -> String -> [((String, String), (String, String, String))] -> ((String, String), (String, String, String))
getTransition position word wordPosition currentState trans = do
	if(position >= length trans)
		then error ("Rejected: " ++ (getWordState word wordPosition currentState ""))
		else trans !! position
	

runTM ::  [((String, String), (String, String, String))] -> String -> String -> String -> String -> Int ->   [String]
runTM trans currentState accept reject word position = do
	let nextInputState = findTransition trans currentState word position
	let goodBool = findTrue nextInputState 0 word position currentState  							--Find True, indicates which transition to use
	let goodTransition = getTransition goodBool word position currentState trans  					--pull out needed transition from list
					
	if(accept == currentState)
		then ("Accept: " ++ getScrubbedWord (getWordState word position accept reject)):[]
		else if (reject == currentState)
			then (word:reject:[])			--TODO: Fix this to return that the word was rejected
			else do
					let newWord = getNewWord word position (getInput goodTransition) 
					let newPosition = position + (positionChange goodTransition)
					let transType = getType goodTransition
					let wordAndState = getWordState word position currentState reject
					let scrubbedWord = getScrubbedWord wordAndState
					scrubbedWord:runTM trans (getNextState goodTransition) accept reject  newWord newPosition
					
					
getScrubbedWord :: String -> String
getScrubbedWord word = delete '_' word
					
				
getWordState :: String -> Int -> String -> String -> String
getWordState word position currentState reject= do
	if(position > length word) 
		then reject
		else do
			let (prefix, rest) = splitAt position word
			if(position == 0)
				then "[" ++ currentState ++ "]" ++ rest
				else prefix ++ "[" ++ currentState ++ "]" ++ rest
	
	
rejectWord:: String -> [String]
rejectWord word = do
	let otherWord = ["The word was rejected!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"]
	word:otherWord
	

getReturnedState :: [(Int, String, String)] -> String
getReturnedState [(_,_, word)] = word

	
getReturnedWord :: [(Int, String, String)] -> String
getReturnedWord [(_,word, _)] = word

getReturnedPosition :: [(Int, String, String)] -> Int
getReturnedPosition [(num,_, _)] = num


			

positionChange :: ((String, String), (String, String, String)) -> Int
positionChange (("rwRt", _),(_,_,_)) = 1
positionChange (("rwLt", _),(_,_,_)) = (-1)
positionChange (("rRl", _),(_,_,_)) = 1
positionChange (("rLl", _),(_,_,_)) = (-1)
positionChange (("rRt", _),(_,_,_)) = 1
positionChange (("rLt", _),(_,_,_)) = (-1)
positionChange ((_, _),(_,_,_)) =  error "Incorrect input"
		

getInput :: ((String, String), (String, String, String)) -> String
getInput ((_,_),(_,input,_)) = input

getInput' :: ((String, String), (String, String, String)) -> String
getInput' ((_,_),(input,_,_)) = input
		
		
getNextState :: ((String, String), (String, String, String)) -> String
getNextState ((_,_),(_,_,nextState)) = nextState

getCurrentState :: ((String, String), (String, String, String)) -> String
getCurrentState ((_,state),(_,_,_)) = state

getType :: ((String, String), (String, String, String)) -> String
getType ((transType, _), (_, _, _)) = transType


		
		
getNewWord :: String -> Int -> String -> String
getNewWord word position write = do
	if(position > length word)
		then error "The word is rejected"
		else do
			let (prefix, rest) = splitAt position word
			if(position == 0)
				then write ++ tail rest
				else if(length rest == 0)
						then prefix ++ write
						else prefix ++ write ++ (tail rest)
	
	
convertToString :: Char -> String
convertToString a = [a]
	


checkTapeInput :: ((String, String), (String, String, String)) -> String -> Int -> Bool
checkTapeInput trans word position = do
	if(position >= length word)
		then error "The word is rejected"
		else if ((getInput' trans) == (convertToString(word !! position)))
				then True
				else False


checkTransState	::((String, String), (String, String, String)) -> String -> Bool
checkTransState trans state = if((getCurrentState trans) == state)
								then True
								else False
	
	
nextTrans :: [((String, String), (String, String, String))] -> ((String, String), (String, String, String))
nextTrans transitions = do
	if(length transitions /= 0)
		then head transitions
		else error "The word is rejected because of an empty head"

	
	
main = do
	
--Get file path and word to test
	putStrLn("Enter the absolute path of your Turing Machine configuration.")
	configPath <- getLine
	putStrLn("What word would you like to test on the Turing Machine?")
	testWord <- getLine
	
--Get file contents
	isFileThere <- doesFileExist configPath
	if(isFileThere)
		then do 
			fileHandle <- openFile configPath ReadMode
			parseFile fileHandle testWord
			--Close handle when done parsing
			hClose fileHandle
		else error "No such file path exists.  Please try a different path."
		
	
	
--end Main
---------------------------------------------------------


--Parsing Input File
------------------------------------------------------------
--TODO: Find a way to check if file is empty and return error
-- to user

parseFile :: Handle -> String -> IO ()
parseFile fileHandle testWord = 
	do fileContents <- hGetContents fileHandle
	   let fileLine = lines fileContents
	   let fileWords = map words fileLine
	   let fileItems = map (map (split (dropBlanks . dropDelims $ oneOf  "{},:;"))) fileWords
	   let verifyNumOfItems = map (map (split (dropBlanks $ oneOf "{},:;"))) fileWords 				--Trying to use this solve TODO #2
	   
	   --Delete empty strings in fileItems
	   let scrubbedData = elimEmpty fileItems
	   --let scrubbedNumOfItems' = elimEmpty verifyNumOfItems
	   
	   --print verifyNumOfItems
	   
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
	   let transBool = (verifyTrans transConstants lsTapeAlpha lsStates lsTransitions)
	   
	   --Data is verified, now run TM on word
	   if(transBool && verifStart && verifAccept && verifAlpha && verifReject)
			then  mapM_  print (runTM lsTransitions sStart sAccept sReject (testWord ++ "_") 0)
			else runError transBool
			
			
		
	   
testFile :: [[String]] -> IO ()
testFile testList2 = mapM_ (mapM_ print) testList2


testList2 = [[1,2,3],[4,5,6]]
testList = [[["--"],["Initialization",""]], [["","states",""],["Q0","Q1","Q2","Q3","Q4","Q5","Q6","Q7","A","R",""]]]                 


--
parseLines' :: [[[String]]] -> [String] -> String -> String -> String -> [String] -> [String] -> [((String, String), (String, String, String))]  -> TM
parseLines' [] states start accept reject alpha tapeAlpha transitions = TM states start accept reject alpha tapeAlpha transitions
parseLines' ([]:rest) states start accept reject alpha tapeAlpha transitions = parseLines' rest states start accept reject alpha tapeAlpha transitions                 		-- Empty line
parseLines' (([]:rest'):rest) states start accept reject alpha tapeAlpha transitions = parseLines' (rest':rest) states start accept reject alpha tapeAlpha transitions 
parseLines' (((e:es):rest'):rest) states start accept reject alpha tapeAlpha transitions | e == "--" = parseLines' (rest':rest)  states start accept reject alpha tapeAlpha transitions
                                                                                         | e == "states" = parseLines' (rest) (head rest') start accept reject alpha tapeAlpha transitions
											 | e == "start" = parseLines' (rest':rest) states (head(head rest')) accept reject alpha tapeAlpha transitions
											 | e == "accept" = parseLines' (rest':rest) states start (head(head rest')) reject alpha tapeAlpha transitions
											 | e == "reject" = parseLines' (rest':rest) states start accept (head(head rest')) alpha tapeAlpha transitions
											 | e == "alpha" = parseLines' (rest) states start accept reject (head rest') tapeAlpha transitions
											 | e == "tape-alpha" = parseLines' (rest) states start accept reject alpha (tapeAlpha++(head rest')) transitions
											 | e == "rwRt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e,(head rest' !! 0)), ((tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0),((tail rest') !! 2 !! 0)))])
											 | e == "rRl" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), (tail rest' !! 0 !! 0,tail rest' !! 0 !! 0,(head rest' !! 0)))])
											 | e == "rRt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), ((tail rest' !! 0 !! 0),(tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0)))])
											 | e == "rwLt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e,(head rest' !! 0)), ((tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0),((tail rest') !! 2 !! 0)))])
											 | e == "rLl" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), (tail rest' !! 0 !! 0,tail rest' !! 0 !! 0,(head rest' !! 0)))])
											 | e == "rLt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), ((tail rest' !! 0 !! 0),(tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0)))])
											 | es == [] = parseLines' (rest':rest) states start accept reject alpha tapeAlpha transitions
											 | otherwise = error "Who goes there? You's bad input, git on outa here!!"
											 --e == "" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha transitions
											 --otherwise = error $ e ++ "\n" ++ (show es) ++ "\n" ++ (show rest') ++ "\n" ++ (show rest)											 
											 --otherwise = parseLines' (rest':rest) states start accept reject alpha tapeAlpha transitions    --throw an error on this line???
											 

--TODO: 1. Potential issue my code doesn't catch when the transition name is not one of the 6 defined types (rLl, etc)
--it just passes over it
--TODO: 2. Code doesn't catch the error where the file may have extra data after a transition
--the code ignores it, should it reject such a file (example: rwLt Q4 1 x Q5 9)?
--TODO: Get rid of empty lines in input file
											 
parseLines :: [[[String]]] -> TM
parseLines f = parseLines' f [] "" "" "" [] ["_"] [] 

--End parsing file
---------------------------------------------------------------

									
--c:\Users\Asus\Desktop\project2Test.txt
--Empty File:
--C:\Users\Asus\Desktop\NewTextDocument(2).txt













