
------------------------- Game data - GoT version

locations :: [Location]
locations =
    [ "Braavos"           -- 0
    , "Castle Black"      -- 1
    , "Kings Landing"     -- 2
    , "Saltpans"          -- 3
    , "The Tree of Life"  -- 4
    , "The Twins"         -- 5
    , "Winterfell"        -- 6
    ]

theMap :: Map
theMap = [(1,4),(1,6),(2,5),(3,5),(5,6)]

characters :: [Party]
characters =
    [ ["Jaqen","a sailor"]                   -- 0 - Braavos
    , ["Jon Snow"]                           -- 1 - Castle Black
    , ["Jaime","King Robert","Queen Cersei"] -- 2 - Kings Landing
    , ["The captain"]                        -- 3 - Saltpans
    , ["The three-eyed raven"]               -- 4 - The Tree of Life
    , ["Walder Frey"]                        -- 5 - The Twins
    , ["Bran","Catelyn","Hodor","Sansa"]     -- 6 - Winterfell
    ]

dialogues :: [(Party,Dialogue)]
dialogues = hotk ++ redwed ++ aryakills ++ ironthrone

--Hand of the King Storyline

hotk :: [(Party,Dialogue)]
hotk = [actQCer,actJam,actCerJam,actKingRob,actBran,actBranCerJam,actNed,actRobNed,actNedH,actJofNed]

actQCer,actJam,actCerJam,actKingRob,actBran,actBranCerJam,actNed,actRobNed,actNedH,actJofNed :: (Party,Dialogue)

actQCer = (["Queen Cersei"], End "Where Robert goes, I go." )

actJam = (["Jaime"], End "As a member of the King's Guard, I will protect King Robert Baratheon." )

actCerJam = (["Jaime","Queen Cersei"], End "[CENSORED]" )

actKingRob = (["King Robert"], Choice "I need a new Hand of the King.  Ned seems like a good choice."
           [("We should head to Winterfell now.",
                Action "My wife Cersei will be coming too. And her brother Jaime, in the King's Guard."
                (update ["Jaime","King Robert", "Queen Cersei"] ["Jaime","King Robert", "Queen Cersei"] [])),
            ("Let's wait a few days.", End "" )])

actBran = (["Bran"], End "There's nothing I love better than climbing around!" )

actBranCerJam = (["Bran", "Jaime", "Queen Cersei"] , Choice "Hmmm, what's going on over there?"
              [("I'll go and have a look" , Action "[CENSORED]. Bran's legs are broken now. Ned turns up to look after Bran."
              (update ["Bran"] [] ["Bran (crippled)","Ned"])),
              ("I better not be nosy, curiosity killed the cat after all.", End "" )])

actNed = (["Ned"], End "The North is such a wonderful place. I never want to leave!" )

actRobNed = (["King Robert", "Ned"], Choice "How about becoming Hand of the King, Ned?"
            [("I'd love to.  The job security appeals to me.", Action "Ned becomes Hand of the King, but Robert is soon killed by a boar and Joffrey becomes King." ((updateAt 2 [] ["Cersei","Joffrey"]) . (update ["Jaime","King Robert","Ned","Queen Cersei"] [] ["Ned (HOTK)"]))),
            ("Hmm, I'm not so sure that's a good idea.", End "" )])

actNedH = (["Ned (HOTK)"] , Choice "I need to get to King's Landing to pledge allegiance to King Joffrey."
     [("I suppose so." , Action "Let's go." (update ["Ned (HOTK)"] ["Ned (HOTK)"] []))
     ,("Wait, this isn't what I signed up for!" , Action "Tough luck, you are coming anyway." (update ["Ned (HOTK)"] ["Ned (HOTK)"] []))
     ])

actJofNed = (["Joffrey","Ned (HOTK)"] , Choice "Admit to your treason!"
    [("Sure." , Action "Ned is sentenced. Stannis invades but Tyrion sets everything on fire. Joffrey's reign ends. The Hound has had enough and wants out of here.  Robb is now head of House Stark, and he is not happy." (update ["Joffrey","Ned (HOTK)"] [] ["The Hound"] . updateAt 6 [] ["Robb"]))
    ,("Not right now." , End "In time, you will." )])

--Red Wedding Storyline

redwed = [actRobb, actCat, actRobbCat, actWal, actRobbWal, actCatWal, actRedWed]

actRobb, actCat, actRobbCat, actWal, actRobbWal, actCatWal, actRedWed :: (Party,Dialogue)

actRobb = (["Robb"] , End "I should probably go to Walder Frey's wedding.  But I'm a bit of a mummy's boy, and don't want to go alone." )

actCat = (["Catelyn"] , End "I don't want to leave Winterfell without Robb.")

actRobbCat = (["Catelyn","Robb"] , Choice "I need Walder Frey's allegiance. Let's accept his wedding invitation and make it a family day out!"
     [("Sure, it'll be good to finally bury the hatchet with House Frey" , Action "Let's go." (update ["Catelyn","Robb"] ["Catelyn","Robb"] []))
     ,("Are you sure this is a good idea? " , Action "Ok." (update ["Catelyn","Robb"] [] ["Catelyn","Robb"]))
     ])

actWal = (["Walder Frey"], End "I'm ever so excited about my daughter's wedding!")

actRobbWal = (["Robb","Walder Frey"] , End "Hmm, isn't Catelyn with you? I wouldn't want her to miss the party...")

actCatWal = (["Catelyn","Walder Frey"] , End "Hmm, isn't Robb with you? It's good to have your friends close!")

actRedWed = (["Catelyn","Robb","Walder Frey"] , Action "The wedding does not end well for Robb and Catelyn. Arya who is hiding outside watches everything."
             (update ["Catelyn","Robb"] [] ["Arya (hiding)"]))

--Arya Super Assassin Storyline

aryakills :: [(Party,Dialogue)]
aryakills = [actHou, actAryaH, actAryaHHou, actArya, actCaptain, actAryaCaptain, actAryaJaq, actAryaA, actAryaSailor, actAryaWal, actAryaCer]


actHou, actAryaH, actAryaHHou, actArya, actCaptain, actAryaCaptain, actAryaJaq, actAryaA, actAryaSailor, actAryaWal, actAryaCer :: (Party,Dialogue)
actHou =  (["The Hound"] , Choice "I wonder where the Stark girls are. Shall we look for them?"
     [("Sure." , Action "Let's go." (update ["The Hound"] ["The Hound"] []))
     ,("Not right now." , Action "Ok." (update ["The Hound"] [] ["The Hound"]))
     ])

actAryaH = (["Arya (hiding)"] , End "I'm not going anywhere!")

actAryaHHou = (["Arya (hiding)","The Hound"] , Action "Come with me if you want to live!" (update ["Arya (hiding)","The Hound"] [] ["Arya"]))

actArya = (["Arya"] , Choice "How will I get my revenge?"
     [("This way." , Action "" (update ["Arya"] ["Arya"] []))
     ,("Not today." , Action "" (update ["Arya"] [] ["Arya"]))
     ])

actCaptain = (["The captain"], End "Look at me ship. Isn't she a beauty! Best of all in Braavos.")

actAryaCaptain = (["Arya","The captain"] , Choice "Well, little girl, what do you want?"
     [("Take me to Braavos." , Choice "I just came from there!"
           [("Here is your coin.", Action "Aye, set sail!" (\ (Game n xs con) -> (updateAt n ["Arya","The captain"] [] . update [] ["Arya"] []) (Game 0 xs con)))])
     ,("Nothing." , End "Go on, off you go then.")
     ])

actAryaJaq = (["Arya","Jaqen"] , Choice "What does a girl want?"
     [("Valar Morgulis.", Action "" (update ["Arya","Jaqen"] [] ["Arya assassin"]))
     ])

actAryaA = (["Arya assassin"] , Choice "Cersei Lannister is still on my list."
     [("This way."  , Action "" (update ["Arya assassin"] ["Arya assassin"] []))
     ,("Not today." , Action "" (update ["Arya assassin"] [] ["Arya assassin"]))
     ])

actAryaSailor =  (["Arya assassin", "a sailor"] , Action "Take me to Westeros" (\(Game n xs con) -> (updateAt n ["Arya assassin","a sailor"] [] . update [] ["Arya assassin"] []) (Game 3 xs con)))

actAryaWal = (["Arya assassin","Walder Frey"] , Action "Now my family have been avenged!" (update ["Walder Frey"] [] []))

actAryaCer = (["Arya assassin","Cersei"] , Action "You were long on my list." (update ["Arya assassin","Cersei"] [] ["The Iron Throne"]))

--Iron Throne Storyline

ironthrone = [actThrone,actBranC,actHod,actWHod,actBranHod,actBranWHod,actRav,actBranHodRav,actVision,actJonS,actJonT,actJonIron,actHodorIron]

actThrone,actBranC,actHod,actWHod,actBranHod,actBranWHod,actRav,actBranHodRav,actVision,actJonS,actJonT,actJonIron,actHodorIron :: (Party,Dialogue)
actThrone = (["The Iron Throne"], End "When you play the game of thrones, you win or you die. There is no middle ground.")

actBranC = (["Bran (crippled)"] , Choice "We need to find the Tree of Life. But I won't be make it without Hodor."
     [("This way."  , Action "" (update ["Bran (crippled)"] ["Bran (crippled)"] []))
     ,("Not today." , Action "" (update ["Bran (crippled)"] [] ["Bran (crippled)"]))
     ])

actHod = (["Hodor"] , Choice "Hodor?"
     [("Come." , Action "Hodor." (update ["Hodor"] ["Hodor"] []))
     ,("Stay." , Action "Hodor." (update ["Hodor"] [] ["Hodor"]))
    ])

actBranHod = (["Bran (crippled)","Hodor"] , Action "Warg. Hodor." (update ["Hodor"] ["Warg Hodor"] []))

actWHod = (["Warg Hodor"], End "Warg Hodor is now controlled by Bran")

actBranWHod = (["Bran (crippled)","Warg Hodor"] , Choice "Hodor!"
     [("Come." , Action "Hodor." (update ["Warg Hodor"] ["Warg Hodor"] []))
     ,("Stay." , Action "Hodor." (update ["Warg Hodor"] [] ["Warg Hodor"]))
    ])

actRav = (["The three-eyed raven"], End "CAW!!!")

actBranHodRav = (["Bran (crippled)","Hodor","The three-eyed raven"], End "Hodor is very loyal, but I wish I had more control over him.  It's dangerous around here!")

actVision = (["Bran (crippled)","The three-eyed raven","Warg Hodor"], Action "In a vision, the three-eyed raven shows Bran the secret of Jon Snow." (update ["Bran (crippled)","The three-eyed raven","Warg Hodor"] [] [] . updateAt 1 ["Jon Snow"] ["Jon Targaryen"]))

actJonS = (["Jon Snow"] , End "I must stay here. It is my duty.")

actJonT = (["Jon Targaryen"] , Choice "Duty, schmuty, let's claim the Iron Throne!"
     [("Sure." , Action "Let's go." (update ["Jon Targaryen"] ["Jon Targaryen"] []))
     ,("Not right now." , Action "Ok." (update ["Jon Targaryen"] [] ["Jon Targaryen"]))
     ])

actJonIron = (["Jon Targaryen","The Iron Throne"] ,Action "Jon claims the Iron Throne, hooks up with his half sister Daenerys, and they all live happily ever after." (\x -> Won))

actHodorIron = (["Hodor", "The Iron Throne"], Action "Hodor, to everyone's surprise, claims the Iron Throne and rules for 40 years with intelligence and discretion." (\x -> Won))
