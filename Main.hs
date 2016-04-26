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
	-- print transAlpha
	-- print transAlpha'
	-- print transStates
	-- print transStates'
	--False `notElem` boolList
	--undefined
-----------------------------------

runError :: Bool -> IO ()
runError a = error "Slow down cowboy, you're riding your horse backwards!"

-- runTM' ::  ((String, String), (String, String, String)) -> String -> String -> String -> String -> Int ->   String
-- runTM' trans currentState accept reject word position = do
	-- let currentTrans =  trans
	-- let retVal = readWrite currentTrans currentState accept reject word position
	-- --decompose retVal to make recursive call with new values	
	-- let newWord = getReturnedWord retVal
	-- let nextState = getReturnedState retVal
	-- let newPosition = getReturnedPosition retVal
	-- getReturnedState retVal
	
findTransition :: [((String, String), (String, String, String))] -> String -> String -> Int -> [Bool]
findTransition trans currentState word position = map (\x -> findTransition' x currentState (word !! position)) trans

getInputState :: ((String, String), (String, String, String)) -> [(String, String, String)]
getInputState ((a,_),(_,b,c)) = [(a,b,c)]

findTransition' :: ((String, String), (String, String, String)) -> String -> Char -> Bool
findTransition' transition state letter = do
	if((getNextState' transition) == state)
		then True
		else False
	
findTrue :: [Bool] -> Int -> Int
findTrue boolList position = do
	if((boolList !! position) == True)
		then position
		else findTrue boolList (position+1)
		
getTransition :: Int -> [((String, String), (String, String, String))] -> ((String, String), (String, String, String))
getTransition position trans = trans !! position
	

runTM ::  [((String, String), (String, String, String))] -> String -> String -> String -> String -> Int ->   [String]
runTM [] currentState accept reject word position = []
runTM trans currentState accept reject word position = do
	if(accept == currentState)
		then []
		else if (reject == currentState)
			then []  --TODO: Fix this to return that the word was rejected
			else do
				let nextInputState = findTransition trans currentState word position
				let goodBool = findTrue nextInputState 0  							--Find True, indicates which transition to use
				let goodTransition = getTransition goodBool trans  					--pull out needed transition from list
				let newWord = getNewWord word position (getInput goodTransition) 
				let newPosition = positionChange goodTransition
				word:runTM trans (getNextState goodTransition) accept reject  newWord newPosition
	--let nextInputAndState = ((getType goodTransition) : (getInput goodTransition) : (getNextState goodTransition) : [])
	
	--print nextInputAndState
	-- let currentTrans = nextTrans trans
	-- let retVal = readWrite currentTrans currentState accept reject word position
	-- --decompose retVal to make recursive call with new values	
	-- let newWord = getReturnedWord retVal
	-- let nextState = getReturnedState retVal
	-- let newPosition = position + (getReturnedPosition retVal)
	
	
	-- if(newPosition == position)
		-- then (runTM (tail trans) currentState accept reject word position)  --Didn't move position
		-- else if (currentState == nextState)
			-- then newWord:(runTM trans nextState accept reject word newPosition)   --Loop on same transition
			-- else newWord : (runTM (tail trans) nextState accept reject newWord newPosition)  --Move to next transition

rejectWord:: String -> [String]
rejectWord word = do
	let otherWord = ["The word was rejected"]
	word:otherWord
	

getReturnedState :: [(Int, String, String)] -> String
getReturnedState [(_,_, word)] = word

	
getReturnedWord :: [(Int, String, String)] -> String
getReturnedWord [(_,word, _)] = word

getReturnedPosition :: [(Int, String, String)] -> Int
getReturnedPosition [(num,_, _)] = num
	
--determine what is read off the tape and written on it	
-- readWrite :: ((String, String), (String, String, String))-> String -> String -> String -> String -> Int -> [(Int, String, String)]
-- readWrite currentTrans currentState accept reject word position = do
	-- let stateCheck = checkTransState currentTrans currentState
	-- let inputCheck = checkTapeInput currentTrans word position
	-- let newPosition = positionChange currentTrans
	-- if(stateCheck && inputCheck)
		-- then [(newPosition,(getNewWord word position (getInput currentTrans)), getNextState currentTrans)] --return position change, new modified word and next state
		-- else [(position, word, currentState)]   --Try the next transition 
		

positionChange :: ((String, String), (String, String, String)) -> Int
positionChange (("rwRt", _),(_,_,_)) = 1
positionChange (("rwLt", _),(_,_,_)) = -1
positionChange (("rRl", _),(_,_,_)) = 1
positionChange (("rLl", _),(_,_,_)) = -1
positionChange (("rRt", _),(_,_,_)) = 1
positionChange (("rLt", _),(_,_,_)) = -1
		

getInput :: ((String, String), (String, String, String)) -> String
getInput ((_,_),(_,input,_)) = input

getInput' :: ((String, String), (String, String, String)) -> String
getInput' ((_,_),(input,_,_)) = input
		
		
getNextState :: ((String, String), (String, String, String)) -> String
getNextState ((_,_),(_,_,nextState)) = nextState

getNextState' :: ((String, String), (String, String, String)) -> String
getNextState' ((_,state),(_,_,_)) = state

getType :: ((String, String), (String, String, String)) -> String
getType ((transType, _), (_, _, _)) = transType


		
		
getNewWord :: String -> Int -> String -> String
getNewWord word position write = do
	let (prefix, rest) = splitAt position word
	if(position == 0)
		then 
			if((length (tail rest)) > 0)
			then write ++ tail rest
			else prefix ++ write
		else(init prefix) ++ write ++ rest
	
	
convertToString :: Char -> String
convertToString a = [a]
	


checkTapeInput :: ((String, String), (String, String, String)) -> String -> Int -> Bool
checkTapeInput trans word position = if ((getInput' trans) == (convertToString(word !! position)))
										then True
										else False


checkTransState	::((String, String), (String, String, String)) -> String -> Bool
checkTransState trans state = if((getNextState' trans) == state)
								then True
								else False
	
	
nextTrans :: [((String, String), (String, String, String))] -> ((String, String), (String, String, String))
nextTrans transitions = head transitions  --make sure list is non-empty

	
	
main = do
--Get file path and word to test
	putStrLn("Enter the absolute path of your Turing Machine configuration.")
	configPath <- getLine
	putStrLn("What word would you like to test on the Turing Machine?")
	testWord <- getLine
	
--Get file contents
	fileHandle <- openFile configPath ReadMode
	parseFile fileHandle testWord
	
--Close handle when done parsing
	hClose fileHandle

--TODO: Find a way to check if file is empty and return error
-- to user
parseFile :: Handle -> String -> IO ()
parseFile fileHandle testWord = 
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

	   let transBool = (verifyTrans transConstants lsTapeAlpha lsStates lsTransitions)
	   if(transBool)
			then print (runTM lsTransitions sStart sAccept sReject testWord 0)
			else runError transBool

	   
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
											 | e == "tape-alpha" = parseLines' (rest':rest) states start accept reject alpha (tapeAlpha++(head rest')) transitions
											 | e == "rwRt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e,(head rest' !! 0)), ((tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0),((tail rest') !! 2 !! 0)))])
											 | e == "rRl" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), (tail rest' !! 0 !! 0,tail rest' !! 0 !! 0,(head rest' !! 0)))])
											 | e == "rRt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), ((tail rest' !! 0 !! 0),(tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0)))])
											 | e == "rwLt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e,(head rest' !! 0)), ((tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0),((tail rest') !! 2 !! 0)))])
											 | e == "rLl" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), (tail rest' !! 0 !! 0,tail rest' !! 0 !! 0,(head rest' !! 0)))])
											 | e == "rLt" = parseLines' (rest':rest) states start accept reject alpha tapeAlpha (transitions ++ [((e, (head rest' !! 0)), ((tail rest' !! 0 !! 0),(tail rest' !! 0 !! 0),((tail rest') !! 1 !! 0)))])
											 | otherwise = parseLines' (rest':rest) states start accept reject alpha tapeAlpha transitions    --throw an error on this line???

--TODO: Potential issue my code doesn't catch when the transition name is one of the 6 defined types (rLl, etc)
--it just passes over it
--TODO: Code doesn't catch the error where the file may have extra data after a transition
--the code ignores it, should it reject such a file (example: rwLt Q4 1 x Q5 9)?
--TODO: Get rid of empty lines in input file
											 
parseLines :: [[[String]]] -> TM
parseLines f = parseLines' f [] "" "" "" [] ["_"] [] 
               
-- parseStates :: [[[String]]] -> [String]
-- parseStates [] = []
-- parseStates [[[xs]]] = [parseStates[[xs]]]
-- parseStates[[xs]] = [parseStates[xs]]
-- parseStates[xs] = case (xs !! 1) of "states" -> xs


									
--c:\Users\Asus\Desktop\project2Test.txt
--Empty File:
--C:\Users\Asus\Desktop\NewTextDocument(2).txt













