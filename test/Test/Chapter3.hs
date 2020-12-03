module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3


chapter3 :: Spec
chapter3 = describe "Chapter3" $ do
    chapter3normal

chapter3normal :: Spec
chapter3normal = describe "Chapter3Normal" $ do
    describe "Task2: Fight" $ do
        describe "fight" $ do
            it "no winner, gold stays the same" $ do
                let knight = Knight' {
                    knightHealth = 4
                    , knightAttack = 1
                    , knightGold = 0
                                    }
                let monster = Monster' {
                    monsterHealth = 2
                    , monsterAttack = 1
                    , monsterGold = 7
                }
                fight' knight monster `shouldBe` 0
            it "knight wins and gets the gold" $ do
                let knight = Knight' {
                    knightHealth = 4
                    , knightAttack = 42
                    , knightGold = 0
                                    }
                let monster = Monster' {
                    monsterHealth = 2
                    , monsterAttack = 1
                    , monsterGold = 7
                }
                fight' knight monster `shouldBe` 7
            it "monster wins" $ do
                let knight = Knight' {
                    knightHealth = 4
                    , knightAttack = 2
                    , knightGold = 0
                                    }
                let monster = Monster' {
                    monsterHealth = 22
                    , monsterAttack = 11
                    , monsterGold = 7
                }
                fight' knight monster `shouldBe` -1
    describe "Task4: City" $ do
        describe "buildHouse" $ do
            it "should append new house to list of houses" $ do
                let city = City {
                    castle = HasCastle "Old Castle" NoWall
                    , building = Church
                    , houses = []
                }
                buildHouse city (House One) `shouldBe` city { houses = [House One]}
        describe "buildWalls" $ do
            it "should build walls if castle and enough people are present" $ do
                let city = City {
                    castle = HasCastle "Old Castle" NoWall
                    , building = Church
                    , houses = replicate 3 (House Four)
                }
                buildWalls city `shouldBe` city { castle = HasCastle "Old Castle" AWall}
            it "should not build walls if no castle is present and enough people are present" $ do
                let city = City {
                    castle = NoCastle
                    , building = Church
                    , houses = replicate 3 (House Four)
                }
                buildWalls city `shouldBe` city { castle = NoCastle}
            it "should not build walls if not enough people are present" $ do
                let city = City {
                    castle = HasCastle "Old Castle" NoWall
                    , building = Church
                    , houses = replicate 2 (House Four)
                }
                buildWalls city `shouldBe` city { castle = HasCastle "Old Castle" NoWall}
        describe "buildCastle" $ do
            it "should build castle if none is present" $ do
                let city = City {
                    castle = NoCastle
                    , building = Church
                    , houses = []
                }
                buildCastle city (HasCastle "My Castle" NoWall) `shouldBe` city { castle = HasCastle "My Castle" NoWall}
            it "should replace castle if one is present" $ do
                let city = City {
                    castle = HasCastle "Old Castle" NoWall
                    , building = Church
                    , houses = []
                }
                buildCastle city (HasCastle "My Castle" NoWall) `shouldBe` city { castle = HasCastle "My Castle" NoWall}
            it "should demolish castle if one is present" $ do
                let city = City {
                    castle = HasCastle "Old Castle" NoWall
                    , building = Church
                    , houses = []
                }
                buildCastle city NoCastle `shouldBe` city { castle = NoCastle}
    describe "Task7: Append" $ do
        describe "Gold'" $ do
            it "should return sum of both golds" $ append (Gold' 3) (Gold' 4) `shouldBe` Gold' 7
        describe "Lists" $ do
            it "should return empty list with two empty lists" $ append ([] :: [Int]) [] `shouldBe` []
            it "should return concatenated lists" $ append [1,2] [3,4] `shouldBe` ([1,2,3,4] :: [Int])
        describe "Maybe" $ do
            it "should return Nothing if first is Nothing" $ append Nothing (Just $ Gold' 3)  `shouldBe` (Nothing :: Maybe Gold')
            it "should return Nothing if second is Nothing" $ append (Just $ Gold' 3) Nothing `shouldBe` (Nothing :: Maybe Gold')
            it "should return append of inner on Just" $ append (Just $ Gold' 3) (Just $ Gold' 4) `shouldBe` (Just $ Gold' 7)
    describe "Task8: Days" $ do
        describe "isWeekend" $ do
            it "should return True on Saturday" $ isWeekend Saturday `shouldBe` True
            it "should return True on Sunday" $ isWeekend Sunday `shouldBe` True
            it "should return False on Monday" $ isWeekend Monday `shouldBe` False
            it "should return False on Tuesday" $ isWeekend Tuesday `shouldBe` False
            it "should return False on Wednesday" $ isWeekend Wednesday `shouldBe` False
            it "should return False on Thursday" $ isWeekend Thursday `shouldBe` False
            it "should return False on Friday" $ isWeekend Friday `shouldBe` False
        describe "nextDay" $ do
            it "should return Tuesday after Monday" $ nextDay Monday `shouldBe` Tuesday
            it "should return Wednesday after Tuesday" $ nextDay Tuesday `shouldBe` Wednesday
            it "should return Thursday after Wednesday" $ nextDay Wednesday `shouldBe` Thursday
            it "should return Friday after Thursday" $ nextDay Thursday `shouldBe` Friday
            it "should return Saturday after Friday" $ nextDay Friday `shouldBe` Saturday
            it "should return Sunday after Saturday" $ nextDay Saturday `shouldBe` Sunday
            it "should return Monday after Sunday" $ nextDay Sunday `shouldBe` Monday
        describe "daysToParty" $ do
            it "should return 4 on Monday" $ daysToParty Monday `shouldBe` 4
            it "should return 3 on Tuesday" $ daysToParty Tuesday `shouldBe` 3
            it "should return 2 on Wednesday" $ daysToParty Wednesday `shouldBe` 2
            it "should return 1 on Thursday" $ daysToParty Thursday `shouldBe` 1
            it "should return 0 on Friday" $ daysToParty Friday `shouldBe` 0
            it "should return 6 on Saturday" $ daysToParty Saturday `shouldBe` 6
            it "should return 5 on Sunday" $ daysToParty Sunday `shouldBe` 5
    describe "Task9: Fight" $ do
        describe "doAttack" $ do
            it "should reduce the monster's hitpoints if a knight attacks" $ do
                let attacker = Knight {
                            kHealth = 3
                            , kAttack = 3
                            , kDefence = 0
                            , kActions = []
                }
                let defender = Monster {
                            mHealth = 2
                            , mAttack = 2
                            , mActions = []
                }

                doAttack attacker defender `shouldBe` (attacker, defender { mHealth = -1 }) 
            it "TODO: negative attack - what should happen?" $ do
                let attacker = Knight {
                            kHealth = 3
                            , kAttack = 3
                            , kDefence = 0
                            , kActions = []
                }
                let defender = Monster {
                            mHealth = 2
                            , mAttack = 2
                            , mActions = []
                }

                doAttack attacker defender `shouldBe` (attacker, defender { mHealth = -123456 }) 
            it "TODO: negative defence - what should happen?" $ do
                let attacker = Knight {
                            kHealth = 3
                            , kAttack = 3
                            , kDefence = 0
                            , kActions = []
                }
                let defender = Monster {
                            mHealth = 2
                            , mAttack = 2
                            , mActions = []
                }

                doAttack attacker defender `shouldBe` (attacker, defender { mHealth = -123456 }) 