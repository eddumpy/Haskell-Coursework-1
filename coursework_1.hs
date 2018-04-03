
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <  y    = x : merge    xs (y:ys)
    | x == y    = x : merge    xs    ys
    | otherwise = y : merge (x:xs)   ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys)
    | x <  y    = x : minus    xs (y:ys)
    | x == y    =     minus    xs    ys
    | otherwise =     minus (x:xs)   ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = msort (take n xs) `merge` msort (drop n xs)
  where
    n = length xs `div` 2

type Node = Int
type Map  = [(Node,Node)]

bothWays :: Node -> Map -> [Node]
bothWays _ [] = []
bothWays n ((x,y):xs)
    | n == x    = y : bothWays n xs
    | n == y    = x : bothWays n xs
    | otherwise =     bothWays n xs

type Location  = String
type Character = String

type Party = [Character]

------------------------- PART 1: Events

data Game  = Won | Game Node Party [Party]
  deriving (Eq,Show)

type Event = Game -> Game

start :: Game
start =  Game 6 [] characters

end :: Game
end = Won

applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt i f xs
    | i >= length xs = error "Index out of bounds."
    | otherwise      = [if n == i then f x else x | (n, x)<- zip [0..] xs]

updateAt :: Node -> Party -> Party -> Event
updateAt m xs ys (Game n p ps) = (Game n p changed)
    where
        (ns, w:ws) = splitAt m ps
        change     = ns ++ [minus (msort w) xs] ++ ws
        changed    = applyAt m (merge ys) change

update :: Party -> Party -> Party -> Event
update xs ys zs (Won)         = Won
update xs ys zs (Game n p ps) = Game n (changed) (updated)
    where
      minused    = minus (msort p) xs
      changed    = merge minused ys
      (ns, w:ws) = splitAt n ps
      remove     = ns ++ [minus (msort w) xs] ++ ws
      updated    = applyAt n (merge zs) remove

------------------------- PART 2: Dialogues

data Dialogue = End     String
              | Choice  String  [( String , Dialogue )]
              | Action  String Event

exitWords = ["X","x","Q","q","Exit","exit","Quit","quit"]

enumerate :: Int -> [String] -> String
enumerate n xs = unlines [ "  " ++ show i ++ ". " ++ x | (i,x) <- zip [n..] xs ]

numberListString :: [String] -> [String]
numberListString [] = []
numberListString xs = [show y | (x,y)<-(zip xs [1..])]

numberListInt :: [String] -> Int -> [Int]
numberListInt [] _  = []
numberListInt xs n  =  [y | (_,y)<-(zip xs [n..])]

getIndex :: String -> [String] -> Int
getIndex _ [] = error "Empty list"
getIndex s xs = z
    where
      zs = [y | (x,y)<-(zip xs [1..]), x == s]
      z  = zs !! ((length zs) - 1)

dialogue :: Game -> Dialogue -> IO Game
dialogue game (End s)       = do
  putStrLn s
  return game
dialogue game (Action s e)  = do
  putStrLn s
  return (e game)
dialogue game (Choice s xs) = do
  putStrLn s
  let ys = [x | (x,_)<-xs]
  let ns = numberListString ys
  putStr (enumerate 1 ys)
  num <- getLine
  if num `elem` exitWords
    then do
        return game
  else if num `elem` ns
    then do
        let zs = [z | (_,z)<-xs]
        let x  = getIndex num ns
        dialogue game (zs !! (x - 1))
  else dialogue game (Choice s xs)

findDialogue :: Party -> Dialogue
findDialogue [] = End "There is nothing we can do."
findDialogue p  = d
    where
      ys = [y | (x,y)<-dialogues, x == p]
      d  = ys !! ((length ys) - 1)

------------------------- PART 3: The game loop

loop :: Game -> IO ()
loop (Won)         = do
  return ()
loop (Game n p ps) = do
  let str = locations !! n
  putStrLn("You are in " ++ str)
  let ns  = bothWays n theMap
  let xs  = [locations !! x | x<-ns]
  putStr("You can travel to: \n" ++ (enumerate 1 xs))
  putStr("With you are: \n")
  putStr(enumerate (length xs + 1) p)
  let zs  = (ps !! n) `minus` p
  putStr("You can see \n" ++ (enumerate (length xs + length p + 1) zs))
  putStrLn("What will you do?")
  str <- getLine
  if str `elem` exitWords
    then do
      putStr ""
    else do
      let ys = words str
      let is = [(read y :: Int ) | y<-ys]
      if length is == 1 && (is !! 0) <= length xs
        then do
          let m = is !! 0
          loop (Game ((bothWays n theMap) !! (m - 1)) p ps)
        else do
          let cs = ps !! n `minus` p
          let js = [j | (i,j) <- (zip [(length xs + 1)..] p) ++ (zip [(length xs + length p + 1)..] cs), i `elem` is ]
          let d  = findDialogue (msort js)
          g <- dialogue (Game n p ps) d
          loop g

game :: IO ()
game = loop start

------------------------- PART 4: Solving the game

-- Function used to accumulate choices made
addNumOfChoices :: Int -> [(Event,[Int])] -> [(Event,[Int])]
addNumOfChoices _ [] = []
addNumOfChoices n xs = [(e, n:ys) | (e, ys)<-xs]

talk' :: Dialogue -> [(Event,[Int])]
talk' (End _)       = []
talk' (Action _ e)  = [(e,[])]
talk' (Choice _ xs) = f [x |(_,x)<-xs] 1
    where
      f :: [Dialogue] -> Int -> [(Event,[Int])]
      f [] _     = []
      f (x:xs) n =  ys' ++ f xs (n + 1)
        where
          ys  = talk' x
          ys' = addNumOfChoices n ys

talk :: Dialogue -> [(Event, String)]
talk d = spoke
    where
      ns    = talk' d
      spoke = [(e, if length xs == 0 then do "" else do "\nIn the dialogue, choose " ++ concat (choices xs)) | (e, xs)<-ns]
          where
            choices :: [Int] -> [String]
            choices []     = []
            choices (x:xs) = (show x ++ " ") : choices xs

event :: String -> Event
event s _ = Game 0 ["Event: " ++ s] []

testDialogue :: Dialogue
testDialogue = Choice "Morpheus opens his palms"
 [("Take the blue pill", Action "" (event "You wake up in bed"))
 ,("Take the red pill",  Action "" (event "You are a battery"))]

testTalk' :: [(Game,[Int])]
testTalk' = [ (e Won,xs) | (e,xs) <- talk' testDialogue]

testTalk :: [(Game,String)]
testTalk = [ (e Won,str) | (e,str) <- talk testDialogue]

-------------------------

extend :: Map -> (Node,[Int]) -> [(Node,[Int])]
extend m (n, xs) = fs
  where
    zs = bothWays n m
    fs = [(z, xs ++ [y]) | (z,y)<-(zip zs [1..])]

checkList :: [(Node,[Int])] -> [Node] -> [(Node,[Int])]
checkList [] []  = []
checkList [] ys  = []
checkList xs []  = xs
checkList ((x,hs):xs) ys
  | x `elem` ys  = checkList xs ys
  | otherwise    = (x,hs) : checkList xs ys

travel' :: Map -> [(Node,[Int])] -> [(Node,[Int])] -> [(Node,[Int])]
travel' _ _ []   = []
travel' m xs zs  = zs ++ travel' m zs (aux zs)
    where
      aux []     = []
      aux (q:qs) = js ++ aux qs
          where
              gs = [x | (x,_)<-xs] ++ [z | (z,_)<-zs]
              ps = extend m q
              js = checkList ps gs

travel :: Map -> Game -> [(Game,String)]
travel m (Won)         = []
travel m (Game n p ps) = options
    where
      ns      = travel' m [] [(n, [])]
      options = [((Game y p ps), if length ys == 0 then do "\nStay In "
        ++ locations !! y else do "\nTravel to "
        ++ locations !! y
        ++ ": " ++ concat (choices ys)) | (y, ys)<-ns]
          where
            choices :: [Int] -> [String]
            choices []     = []
            choices (x:xs) = (show x ++ " ") : choices xs
-------------------------

contains :: Party -> Party -> Bool
contains [] ys   = True
contains (x:xs) ys
   | x `elem` ys = contains xs ys
   | otherwise   = False

act :: Game -> [(Game, String)]
act Won           = []
act (Game n p ps) = options
    where
      ns      = [(zs, talk d) | (zs, d)<-dialogues, contains zs (p ++ (ps !! n))]
      options = f ns
        where
          f []           = []
          f ((ds, t):ts) = [(e (Game n p ps), "Talk to " ++ (lts  (addAnd ds)) ++ s) | (e, s)<-t, suitable (Game n p ps) e] ++ f ts
              where
                lts []       = []
                lts (d:ds)   =  (d ++ " ") ++ lts ds

-- Function used for formatting reasons for the solve function
addAnd :: [String] -> [String]
addAnd [] = []
addAnd xs = if length xs > 1 then do added else do xs
   where
     (ns, f) = splitAt (length xs - 1) xs
     added = (if length ns > 1 then do (commas ns) else do ns) ++ ["and"] ++ f
         where
           commas :: [String] -> [String]
           commas [] = []
           commas (x:xs) = (x ++ ",") : commas xs

suitable :: Game -> Event -> Bool
suitable (Won) e         = True
suitable (Game n p ps) e = choose
    where
      game   = e (Game n p ps)
      choose = check game
          where
            check (Won)                   = True
            check (Game m q qs)
              | length q > length p       = True
              | (length (minus q p)) > 0  = True
              | length (msort ((concat characters) ++ (concat qs) ++ p ++ q)) > length (msort ((concat characters) ++ (concat ps) ++ p ++ q)) = True
              | otherwise                 = False

solve :: IO ()
solve = do
  putStrLn(solveLoop (start, ""))
    where
      solveLoop :: (Game,String) -> String
      solveLoop (Won, s)            = s
      solveLoop ((Game n p ps), st) = route
            where
              gs     = (travel theMap (Game n p ps))
              rs     = concat (routes gs)
              (g,s)  =  (rs !! 0)
              i      = getIndexFS 0 (getNode g) (listNodes gs)
              (_,ss) = (gs !! i)
              str    =  st ++ ss ++ "\n" ++ s
              route  = solveLoop (g, str)

-- Gets index For Solve
getIndexFS :: Int -> Int -> [Int] -> Int
getIndexFS _ _ [] = 0
getIndexFS x y (z:zs)
   | y == z    = x
   | otherwise = getIndexFS (x + 1) y zs

-- Gets the node of the Game
getNode :: Game -> Node
getNode Won = (-1)
getNode (Game n p ps) = n

-- Returns a list of nodes
listNodes :: [(Game,String)] -> [Node]
listNodes []       = []
listNodes ((g,s):gs) = checkGame g : listNodes gs
    where
      checkGame Won           = (-1)
      checkGame (Game n p ps) = n

routes :: [(Game,String)] -> [[(Game,String)]]
routes []          = []
routes ((g, s):xs) = act g : routes xs

------------------------- Game data

characters :: [Party]
characters =
  [ ["Duke"]
  , ["Portal Gun"]
  , ["Priest"]
  , ["Lee"]
  , ["Chell","Cortana","Mario","Master Chief"]
  , ["Team Rocket"]
  , ["Peach","Rochelle"]
  ]

locations :: [Location]
locations =
  [ "You are not supposed to be here" -- 0
  , "Aperture Science" -- 1
  , "Church of Halo"   -- 2
  , "Macon"            -- 3
  , "Nintendo Land"    -- 4
  , "Pallet Town"      -- 5
  , "Princess Castle"  -- 6
  ]

theMap :: Map
theMap = [(1,5), (2,4), (2,6), (3,5), (4,5), (4,6)]

dialogues :: [(Party,Dialogue)]
dialogues =
 [ (["Mario"] , Choice "I need to save the Princess."
     [("Sure." ,          Action "Let's go." (update ["Mario"] ["Mario"] []))
     ,("Not right now." , Action "Ok."       (update ["Mario"] [] ["Mario"]))
     ])
 , (["Mario","Peach"] , Choice "Save me, Mario!"
    [("Sure." , Action "Thank you for bringing me my hero. Now I can conveniently leave this hat behind." (update ["Mario","Peach"] [] ["Baseball Cap"]))
    ,("Not right now." , End "Mario, pls.")])
 , (["Peach"] , End "That's *Princess* Peach to you, please. And where's my Mario?")
 , (["Master Chief"] , Choice "I want to marry Cortana. Can you escort us to the Church of Halo?"
     [("Sure." ,          Action "Let's go." (update ["Master Chief"] ["Master Chief"] []))
     ,("Not right now." , Action "Ok."       (update ["Master Chief"] [] ["Master Chief"]))
     ])
 , (["Cortana"] , Choice "I must go with Master Chief."
     [("Sure." ,          Action "Let's go." (update ["Cortana"] ["Cortana"] []))
     ,("Not right now." , Action "Ok."       (update ["Cortana"] [] ["Cortana"]))
     ])
 , (["Master Chief","Priest"] , End "I can't marry you without your bride-to-be.")
 , (["Cortana","Priest"] , End "I can't marry you without your husband-to-be.")
 , (["Priest"] , Choice "Welcome, my child. Have you accepted Master Chief as your savior?"
     [("Hail Master Chief (Blessed Be His Name)" , End "")])
 , (["Cortana","Master Chief","Priest"] , Choice "Do you, Master Chief, accept Cortana to be your beloved bride?"
      [("I don't", End "The Wedding is cancelled"),
       ("I do", Choice "And do you, Cortana, take Master Chief to be your beloved Husband?"
        [("I don't", End "The Wedding is cancelled"),
         ("I do", Action "What a beautiful wedding said the bridesmaid to the waiter. But what a shame, that there's some child lurking nearby." (update ["Cortana","Master Chief","Priest"] [] ["Clementine (hiding)"]) )
   ])])
 , (["Baseball Cap"] , Choice "It's a bit grubby, shall I take it?"
     [("Sure." ,          Action "Let's go." (update ["Baseball Cap"] ["Baseball Cap"] []))
     ,("Not right now." , Action "Ok."       (update ["Baseball Cap"] [] ["Baseball Cap"]))
     ])
 , (["Clementine (hiding)"] , End "I'm scared. Where are my parents?")
 , (["Baseball Cap", "Clementine (hiding)"] , Choice "Give the girl the hat?"
    [("Sure." , Action "I feel safe." (update ["Baseball Cap","Clementine (hiding)"] ["Clementine"] []))
    ,("Not right now." , End "")
    ])
 , (["Duke"] , End "Time to k*** a** and chew bubble gum. And I'm all outta gum.")
 , (["Clementine"] , Choice "Will you help me find my parents?"
      [("What do they look like?", Choice "My father's name is Lee"
        [("I asked what do they look like!", End "Sorry")
        ,("I know him, Let's go!", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
      ])
      ,("Do you know your address?", Choice "I can't remember, I think it rhymes with Bacon"
        [("How do you not know your own address?", End "Sorry!"),
         ("Are you thinking of Macon?", Choice "Yes! That's it! Do you know where it is?"
          [("Sure, I can take you there", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
          ,("I don't know how to get there", End "Okay then")
          ])
        ,("Are you hungry?", Choice "Yes! Do you have any chocolate?"
          [("I don't", End "Okay")
          ,("I can go find some", Action "Thanks, I'll stay here in case my parents come back" (update ["Clementine"] [] ["Clementine"]))
   ])])])
 , (["Clementine","Lee"] , Choice "GIVE ME BACK CLEMENTINE!"
     [("Sure...", Action "" (update ["Clementine","Lee"] ["Zombie Lee"] []))
     ])
 , (["Lee"] , End "Clem? Clem, where are you?!")
 , (["Zombie Lee"] , Choice "Uuurrurrhgghghhghgg."
     [("This way." ,  Action "Urg" (update ["Zombie Lee"] ["Zombie Lee"] []))
     ,("Not today." , Action "Hhuuuurgh" (update ["Zombie Lee"] [] ["Zombie Lee"]))
     ])
 , (["Rochelle"] , End "Girl, you should pray there aren't no Zombies around.")
 , (["Rochelle", "Zombie Lee"] , Action "What?! A zombie? You've left me for dead!" (update ["Rochelle","Zombie Lee"] [] ["Pikachu"]))
 , (["Chell"] , Choice "I've just got a volunteering position at Aperture Science. Can you help me find it? I'm not good with directions."
     [("This way." ,  Action "" (update ["Chell"] ["Chell"] []))
     ,("Not today." , Action "" (update ["Chell"] [] ["Chell"]))
    ])
 , (["Chell","Portal Gun"] , Action "This is your fault. It didn't have to be like this. I'm not kidding, now! Turn back, or I will kill you! I'm going to kill you, and all the cake is gone! You don't even care, do you? This is your last chance! ." (update ["Chell","Portal Gun"] [] [] . updateAt 4 ["Team Rocket"] ["Ash"]))
 , (["Team Rocket"] , End "Oh, prepare for trouble, that's what they should do. And make it double, we're grabbing Pikachu.")
 , (["Pikachu"] , Choice "Pika-Pika"
     [("*throw pokeball*"  , Action "" (update ["Pikachu"] ["Pikachu"] []))
     ,("Nope." ,             Action "" (update ["Pikachu"] [] ["Pikachu"]))
     ])
 , (["Ash", "Pikachu"] , Action "You win." (\_ -> Won))
 , (["Pikachu","Team Rocket"] , End "Hey, look at this! Get a load! Let's grab- ALL GLORY TO THE HYPNOTOAD")
 , (["Portal Gun"] , End "I am an inanimate object. What did you expect?")
 ]
