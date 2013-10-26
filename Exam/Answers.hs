module Answers

where 

import Assert
import Data.List(sort, nub)
import System.Random

-- pretty tree from: http://hackage.haskell.org/package/pretty-tree
import qualified Data.Tree as T
import Data.Tree.Pretty

f :: (Integer,Integer) -> (Integer,Integer)
f = until (odd.snd) (\ (m,n) -> (m+1,n `div` 2))

{--
    Question 1: Consider the following function:
    --
    Write an assertive version of this by filling out the dots in the following defnition. Next explain.
--}

fA = assert1 (\ (s,r) (t,u) -> (u*2^(t-s)) == r) f

{--
    First I tried to find out what the function does. 
        Untill the second item is odd, 
        add one to the first number and divide the second by 2.
            So: (1,2) will result in (2,1).

    So, t is s + the number of times s can be divided by 2.
        r is the results of the last division
    This means that u*2^(t-s) == r
        where u = the result
            t-s = the number of times divided by 2

    The test below: fATest will not throw an 'assert error'
--}
fATest = map fA [(x,y) | x <- [1..100], y <- [1..100]]

{--
    Question 2: 
        Implement a function bintree2btree :: a -> BinTree a -> Btree a that converts a BinTree to a
        Btree by throwing away internal node information and insertion of copies of the first argument
        at the leaves. So bintree2bree x should insert copies of x at the leaf nodes.

        Implement a function btree2bintree :: a -> Btree a -> BinTree a that converts a Btree
        to a BinTree by throwing away the leaf information, and filling up the internal nodes with copies
        of the fist argument. So btree2bintree x should insert copies of x at the internal nodes.
--}

data BinTree a = Nil | B a (BinTree a) (BinTree a) deriving (Eq,Show)

data Btree a = Leaf a | Node (Btree a) (Btree a) deriving (Eq,Show)

type Dict = BinTree (String,String) 

key, value :: (String,String) -> String
key (x,_) = x
value  (_,y) = y 

exampleBinTree1 = (Nil)
exampleBinTree2 = (B 1 (Nil) (Nil))
exampleBinTree3 = (B 10 (exampleBinTree2) (exampleBinTree2))
exampleBinTree5 = (B 10 (exampleBinTree3) (exampleBinTree3))
exampleBinTree6 = (B 10 (exampleBinTree3) (exampleBinTree5))

bintree2btree :: a -> BinTree a -> Btree a
bintree2btree x Nil = (Leaf x)
bintree2btree x (B _ t1 t2) = (Node (bintree2btree x t1) (bintree2btree x t2))

bintree2btreeA = assert2 (\_ bintree btree -> (depthBin bintree) == (depthB btree)) bintree2btree

exampleBtree1 = (Leaf 2)
exampleBtree2 = (Node (Leaf 1) (Leaf 2))
exampleBtree3 = (Node (exampleBtree2) (exampleBtree2))
exampleBtree4 = (Node (exampleBtree3) (exampleBtree3))
exampleBtree5 = (Node (exampleBtree3) (exampleBtree4))

btree2bintree :: a -> Btree a -> BinTree a
btree2bintree x (Leaf _) = Nil
btree2bintree x (Node t1 t2) = B x (btree2bintree x t1) (btree2bintree x t2)

btree2bintreeA = assert2 (\_ btree bintree -> (depthB btree) == (depthBin bintree)) btree2bintree

depthBin :: BinTree a -> Int
depthBin Nil = 0
depthBin (B _ t1 t2) = max (depthBin t1) (depthBin t2) + 1

depthB :: Btree a -> Int
depthB (Leaf _) = 0
depthB (Node t1 t2) = max (depthB t1) (depthB t2) + 1

{--
    You can test this by checking for valid input and output.
    In the bin2b, all input elements must be BinTrees and output of Btrees.
    and in b2bin, all input elements must be Btrees and output of BinTrees.

    Result:
        *Answers> exampleBtree1
        Leaf 2
        (0.00 secs, 518324 bytes)
        *Answers> btree2bintree 4 exampleBtree1
        B 4 Nil Nil
        (0.02 secs, 1605196 bytes)
    -- Leaf 2 is transformed into B 4 Nil Nil.

        *Answers> exampleBinTree2
        B 1 Nil Nil
        (0.00 secs, 515640 bytes)
        *Answers> bintree2btree 5 exampleBinTree2
        Node (Leaf 5) (Leaf 5)
        (0.00 secs, 513900 bytes)
    -- B 1 Nil Nil is transformed into Node (Leaf 5) (Leaf 5)

--}
{--
    Additions after the exam:
    -- depth functions
    -- assertions
        btree2bintree 4 (bintree2btree 4 (btree2bintree 4 exampleBtree2))
        B 4 Nil Nil

        *Answers> btree2bintreeA 4 (bintree2btreeA 4 (btree2bintreeA 4 exampleBtree2))
        B 4 Nil Nil
        (0.00 secs, 518492 bytes)
        *Answers> btree2bintreeA 4 (bintree2btreeA 4 (btree2bintreeA 4 exampleBtree3))
        B 4 (B 4 Nil Nil) (B 4 Nil Nil)
        (0.00 secs, 518484 bytes)
        *Answers> btree2bintreeA 4 (bintree2btreeA 4 (btree2bintreeA 4 exampleBtree1))
        Nil
        (0.02 secs, 1036384 bytes)


--}

{-- 
    Question 3: Implement a function

--}

inOrder :: BinTree a -> [a]
inOrder Nil = []
inOrder (B x t1 t2) = (inOrder t1) ++ [x] ++ (inOrder t2)

-- tree made according to an image like: http://1.bp.blogspot.com/-Kog2XdPbTuA/UV29A7m5_PI/AAAAAAAAAJc/NHX3LSdtqmY/s320/diahid.JPG
exampleBinTree4 = (B 4 (B 2 (B 1 Nil Nil) (B 3 Nil Nil)) (B 6 (B 5 Nil Nil) (B 7 Nil Nil)))
-- result = *Answers> inOrder exampleBinTree4
--                    [1,2,3,4,5,6,7]

inOrderRev :: BinTree a -> [a]
inOrderRev Nil = []
inOrderRev (B x t1 t2) = (inOrderRev t2) ++ [x] ++ (inOrderRev t1)
--inOrderRev = reverse . inOrder <-- THIS IS OFCOURSE NOT VALID!!!

treeProperty :: Eq a => BinTree a -> Bool
treeProperty t = inOrder t == reverse (inOrderRev t)

-- Results:
--    *Answers> treeProperty exampleBinTree4
--    True
--    (0.00 secs, 517700 bytes)
--    *Answers> treeProperty exampleBinTree3
--    True
--    (0.00 secs, 520416 bytes)
--    *Answers> treeProperty exampleBinTree2
--    True
--    (0.00 secs, 519932 bytes)
--    *Answers> treeProperty exampleBinTree1
--    True
--    (0.00 secs, 1034320 bytes)

{--
    Question 4:
--}

exampleOrderedDict1 = B ("key", "value") Nil Nil
exampleOrderedDict2 = (B ("d", "4") (B ("b", "2") (B ("a", "1") Nil Nil) (B ("c", "3") Nil Nil)) (B ("f", "6") (B ("e", "5") Nil Nil) (B ("g", "7") Nil Nil)))

exampleUnorderedDict1 = (B ("z", "26") (B ("b", "2") (B ("a", "1") Nil Nil) (B ("c", "3") Nil Nil)) (B ("f", "6") (B ("e", "5") Nil Nil) (B ("g", "7") Nil Nil)))
exampleUnorderedDict2 = (B ("z", "26") (B ("b", "2") (B ("a", "1") Nil Nil) (B ("c", "3") Nil Nil)) (B ("f", "6") (B ("y", "25") Nil Nil) (B ("g", "7") Nil Nil)))
exampleUnorderedDict3 = (B ("Foo", "Bar") exampleUnorderedDict1 exampleUnorderedDict2)

emptyDict = Nil

ordered :: Dict -> Bool
ordered x = sortedList == list
    where sortedList = sort list;
                list = map fst (inOrder x)

-- added noDupes later on.
noDupes :: Dict -> Bool
noDupes x = nubList == list
    where nubList = nub (list);
             list = map fst (inOrder x)

-- added dictLength to check if the size is the same or +1.
-- added after the exam
dictLength :: Dict -> Int
dictLength x = length (map fst (inOrder x))

    
-- Result:
--    *Answers> ordered exampleOrderedDict1
--    True
--    (0.00 secs, 516352 bytes)
--    *Answers> ordered exampleOrderedDict2
--    True
--    (0.00 secs, 516932 bytes)
--    *Answers> ordered exampleUnorderedDict1
--    False
--    (0.00 secs, 1035100 bytes)
--    *Answers> ordered exampleUnorderedDict2
--    False
--    (0.02 secs, 515260 bytes)
--    *Answers> ordered exampleUnorderedDict3
--    False
--    (0.00 secs, 1031948 bytes)

{--
    Question 5:
        Setup for the search:
        - Compare the search string s with the key k of the node
        - if EQ -> return key
        - if s LT k -> search Ltree
        - if otherwise -> search Rtree
--}

lookUp :: String -> Dict -> [String]
lookUp s Nil = []
lookUp s (B x lTree rTree)
    | s == key(x) = [value(x)]
    | s <  key(x) = lookUp s lTree
    | otherwise   = lookUp s rTree

{--
    Hand tested results:

        *Answers> lookUp "b" exampleOrderedDict2
        ["2"]
        (0.00 secs, 514340 bytes)
        *Answers> lookUp "c" exampleOrderedDict2
        ["3"]
        (0.00 secs, 516236 bytes)
        *Answers> lookUp "d" exampleOrderedDict2
        ["4"]
        (0.00 secs, 516992 bytes)
        *Answers> lookUp "f" exampleOrderedDict2
        ["6"]
        (0.00 secs, 516968 bytes)
        *Answers> lookUp "f" exampleOrderedDict1
        []
        (0.00 secs, 517116 bytes)
        *Answers> lookUp "key" exampleOrderedDict1
        ["value"]
        (0.00 secs, 515928 bytes)
--}

{-- 
    Question 6:

--}

insertKey :: (String,String) -> Dict -> Dict
insertKey s Nil = B s Nil Nil
insertKey s (B x lTree rTree)
    | key(s) == key(x) = B s lTree rTree
    | key(s) <  key(x) = B x (insertKey s lTree) rTree
    | otherwise   = B x lTree (insertKey s rTree)

lengthProp :: (String,String) -> Dict -> Dict -> Bool
lengthProp s dictIn dictOut 
    | exists    = (dictLength dictIn)+0 == dictLength dictOut
    | otherwise = (dictLength dictIn)+1 == dictLength dictOut
    where exists = (length (lookUp (key s) dictIn)) == 1

sortedProp :: (String,String) -> Dict -> Dict -> Bool
sortedProp _ dictIn dictOut = 
        ordered dictIn ==> ordered dictOut

noDupesProp :: (String,String) -> Dict -> Dict -> Bool
noDupesProp _ dictIn dictOut =
        noDupes dictIn ==> noDupes dictOut

lookupProp :: (String,String) -> Dict -> Dict -> Bool
lookupProp s dictIn dictOut = (length (lookUp (key s) dictOut)) == 1

insertKeyA = assert2 lengthProp 
           $ assert2 sortedProp 
           $ assert2 noDupesProp 
           $ assert2 lookupProp
                insertKey
{-- 
    Manual test results:
        *Answers> insertKey ("key","value1") exampleOrderedDict1
        B ("key","value1") Nil Nil
        (0.00 secs, 517356 bytes)

        *Answers> inOrder(insertKey ("kea","value1") exampleOrderedDict1)
        [("kea","value1"),("key","value")]
        (0.00 secs, 516904 bytes)
        *Answers> inOrder(insertKey ("kez","value1") exampleOrderedDict1)
        [("key","value"),("kez","value1")]
        (0.00 secs, 517356 bytes)

--}

-- write tests to insert X random strings.
testInsertRandom :: Dict -> Int -> IO Dict
testInsertRandom dict n = testInsertRandom' dict 0 n

testInsertRandom' :: Dict -> Int -> Int -> IO Dict
testInsertRandom' dict n 0 = return dict
testInsertRandom' dict n m = do
    k <- getRandomString;
    testInsertRandom' (insertKeyA (k,k ++ "-value") dict) (n+1) (m-1)

testInsert :: Dict -> Int -> IO ()
testInsert dict n = testInsert' dict 0 n

testInsert' :: Dict -> Int -> Int -> IO ()
testInsert' dict n 0 = putStrLn ("Ran " ++ show n ++ " successful.");
testInsert' dict n m = do 
    x <- getRandomInt 10 40
    dict' <- testInsertRandom dict x
    printtree dict'
    print (map key (inOrder (dict')))
    putStrLn ("Passed.")
    testInsert' dict (n+1) (m-1)


{-- 
    Test results of the function:
    -- Prints: dict : inOrder : passed

        *Answers> testInsertSorted exampleOrderedDict2 1
        B ("d","4") (B ("b","2") (B ("a","1") Nil (B ("asom","asom-value") Nil Nil)) (B ("c","3") Nil Nil)) (B ("f","6") (B ("e","5") (B ("dgls","dgls-value") Nil Nil) Nil) (B ("g","7") Nil (B ("skoh","skoh-value") (B ("qbfr","qbfr-value") (B ("olii","olii-value") Nil Nil) Nil) (B ("xufx","xufx-value") (B ("xrhz","xrhz-value") (B ("ufuz","ufuz-value") (B ("tosw","tosw-value") Nil Nil) Nil) Nil) (B ("zfzw","zfzw-value") Nil Nil)))))
        ["a","asom","b","c","d","dgls","e","f","g","olii","qbfr","skoh","tosw","ufuz","xrhz","xufx","zfzw"]
        Passed.
        Ran 1 successful.
        (0.02 secs, 1630840 bytes)

        *Answers> testInsertSorted exampleOrderedDict2 1000
        ...
        Passed.
        Ran 1000 successful.

NEXT:
    -- Added "noDupes (dict')" on line: "if ordered (dict') && noDupes (dict')"
        AND changed the atoZ to "atoZ = [ x | x <- ['a'] ]" so it only generates aaaa
    -- The result is that is produces no duplicates.

        B ("d","4") (B ("b","2") (B ("a","1") Nil (B ("aaaa","aaaa-value") Nil Nil)) (B ("c","3") Nil Nil)) (B ("f","6") (B ("e","5") Nil Nil) (B ("g","7") Nil Nil))
        ["a","aaaa","b","c","d","e","f","g"]
        Passed.
        Ran 100 successful.
        (0.09 secs, 14495524 bytes)

NEXT:
    -- Changing the number of insertions to 100 works great:

        B ("d","4") (B ("b","2") (B ("a","1") Nil (B ("ascp","ascp-value") (B ("ahjn","ahjn-value") (B ("admx","admx-value") Nil Nil) Nil) (B ("axpt","axpt-value") Nil Nil))) (B ("c","3") (B ("bmqf","bmqf-value") (B ("bifx","bifx-value") Nil Nil) (B ("bsos","bsos-value") Nil Nil)) (B ("cwsu","cwsu-value") (B ("ckun","ckun-value") (B ("cbrm","cbrm-value") Nil Nil) (B ("clxa","clxa-value") Nil Nil)) Nil))) (B ("f","6") (B ("e","5") (B ("duzt","duzt-value") (B ("dllb","dllb-value") Nil (B ("dlzb","dlzb-value") Nil (B ("dprc","dprc-value") Nil Nil))) (B ("dwhb","dwhb-value") (B ("dwar","dwar-value") Nil Nil) Nil)) (B ("enkg","enkg-value") (B ("eeda","eeda-value") Nil Nil) (B ("eqjs","eqjs-value") Nil (B ("evfh","evfh-value") (B ("erhn","erhn-value") (B ("eren","eren-value") Nil Nil) Nil) Nil)))) (B ("g","7") (B ("fpie","fpie-value") (B ("fnlu","fnlu-value") Nil Nil) Nil) (B ("zgpd","zgpd-value") (B ("zbfm","zbfm-value") (B ("ktla","ktla-value") (B ("ijur","ijur-value") (B ("ihny","ihny-value") (B ("gnas","gnas-value") Nil (B ("grbd","grbd-value") Nil (B ("grzq","grzq-value") Nil (B ("hixc","hixc-value") Nil (B ("htdu","htdu-value") (B ("hjqi","hjqi-value") Nil Nil) (B ("iesl","iesl-value") Nil Nil)))))) Nil) (B ("krqb","krqb-value") (B ("jcyn","jcyn-value") (B ("iznp","iznp-value") (B ("imrb","imrb-value") Nil Nil) Nil) (B ("jeyj","jeyj-value") Nil (B ("kjgu","kjgu-value") (B ("jviy","jviy-value") Nil (B ("jypd","jypd-value") Nil (B ("kilp","kilp-value") Nil Nil))) (B ("krbq","krbq-value") Nil Nil)))) Nil)) (B ("qvwb","qvwb-value") (B ("pwmw","pwmw-value") (B ("luri","luri-value") (B ("llic","llic-value") (B ("kwtb","kwtb-value") (B ("kvsq","kvsq-value") Nil Nil) Nil) Nil) (B ("mlbm","mlbm-value") (B ("lxie","lxie-value") Nil (B ("mkrl","mkrl-value") (B ("mhon","mhon-value") (B ("mcqz","mcqz-value") Nil (B ("mcyr","mcyr-value") Nil Nil)) Nil) Nil)) (B ("okhm","okhm-value") (B ("niyb","niyb-value") (B ("mtnb","mtnb-value") (B ("mmpl","mmpl-value") Nil (B ("mnyg","mnyg-value") Nil Nil)) (B ("mzvw","mzvw-value") Nil (B ("nbxa","nbxa-value") Nil Nil))) (B ("nols","nols-value") Nil (B ("oebq","oebq-value") Nil Nil))) (B ("psfj","psfj-value") (B ("pqff","pqff-value") (B ("ophr","ophr-value") Nil (B ("oyto","oyto-value") Nil (B ("peuz","peuz-value") Nil Nil))) Nil) Nil)))) (B ("qaaq","qaaq-value") Nil Nil)) (B ("vimt","vimt-value") (B ("unfx","unfx-value") (B ("rijr","rijr-value") (B ("rfab","rfab-value") Nil Nil) (B ("stkj","stkj-value") (B ("rpyz","rpyz-value") Nil Nil) (B ("ucyc","ucyc-value") (B ("ttof","ttof-value") Nil Nil) (B ("uixw","uixw-value") Nil Nil)))) Nil) (B ("xizl","xizl-value") (B ("vqii","vqii-value") Nil (B ("wrcd","wrcd-value") (B ("vutq","vutq-value") Nil (B ("wixo","wixo-value") Nil (B ("wltx","wltx-value") Nil Nil))) (B ("xakt","xakt-value") Nil Nil))) (B ("yolm","yolm-value") (B ("ylqt","ylqt-value") (B ("yanm","yanm-value") (B ("xjyj","xjyj-value") Nil Nil) Nil) Nil) Nil))))) (B ("zdvn","zdvn-value") Nil Nil)) (B ("zrrz","zrrz-value") (B ("zhzk","zhzk-value") Nil (B ("zrqg","zrqg-value") (B ("zplh","zplh-value") Nil Nil) Nil)) (B ("zuvw","zuvw-value") Nil Nil)))))
        ["a","admx","ahjn","ascp","axpt","b","bifx","bmqf","bsos","c","cbrm","ckun","clxa","cwsu","d","dllb","dlzb","dprc","duzt","dwar","dwhb","e","eeda","enkg","eqjs","eren","erhn","evfh","f","fnlu","fpie","g","gnas","grbd","grzq","hixc","hjqi","htdu","iesl","ihny","ijur","imrb","iznp","jcyn","jeyj","jviy","jypd","kilp","kjgu","krbq","krqb","ktla","kvsq","kwtb","llic","luri","lxie","mcqz","mcyr","mhon","mkrl","mlbm","mmpl","mnyg","mtnb","mzvw","nbxa","niyb","nols","oebq","okhm","ophr","oyto","peuz","pqff","psfj","pwmw","qaaq","qvwb","rfab","rijr","rpyz","stkj","ttof","ucyc","uixw","unfx","vimt","vqii","vutq","wixo","wltx","wrcd","xakt","xizl","xjyj","yanm","ylqt","yolm","zbfm","zdvn","zgpd","zhzk","zplh","zrqg","zrrz","zuvw"]
        Passed.
        Ran 10 successful.
        (0.17 secs, 25957420 bytes)

LAST: added empty Dict

        *Answers> testInsertSorted emptyDict 10
        B ("yjne","yjne-value") (B ("qodk","qodk-value") (B ("auvq","auvq-value") Nil (B ("iajq","iajq-value") (B ("flki","flki-value") Nil Nil) (B ("omcw","omcw-value") Nil Nil))) (B ("yiiy","yiiy-value") (B ("wnpx","wnpx-value") (B ("tgpp","tgpp-value") Nil Nil) Nil) Nil)) (B ("ynsd","ynsd-value") Nil Nil)
        ["auvq","flki","iajq","omcw","qodk","tgpp","wnpx","yiiy","yjne","ynsd"]

        Passed.
        B ("srpy","srpy-value") (B ("quxf","quxf-value") (B ("aqfw","aqfw-value") (B ("agnx","agnx-value") Nil Nil) (B ("dhwd","dhwd-value") Nil (B ("ftbv","ftbv-value") Nil (B ("phcr","phcr-value") (B ("loho","loho-value") Nil Nil) Nil)))) Nil) (B ("uydr","uydr-value") Nil (B ("yyry","yyry-value") Nil Nil))
        ["agnx","aqfw","dhwd","ftbv","loho","phcr","quxf","srpy","uydr","yyry"]
        Passed.
        B ("rxzl","rxzl-value") (B ("hlck","hlck-value") (B ("bvgg","bvgg-value") (B ("bhoh","bhoh-value") Nil Nil) Nil) (B ("prns","prns-value") (B ("ihfk","ihfk-value") Nil Nil) (B ("qusr","qusr-value") Nil Nil))) (B ("tadm","tadm-value") Nil (B ("wudj","wudj-value") (B ("wjnz","wjnz-value") Nil Nil) Nil))
        ["bhoh","bvgg","hlck","ihfk","prns","qusr","rxzl","tadm","wjnz","wudj"]
        Passed.
        B ("tydr","tydr-value") (B ("kzfe","kzfe-value") (B ("gnhq","gnhq-value") (B ("btmo","btmo-value") Nil Nil) (B ("kslr","kslr-value") (B ("jttl","jttl-value") Nil Nil) Nil)) (B ("mfmv","mfmv-value") Nil (B ("mvvn","mvvn-value") Nil (B ("ssja","ssja-value") Nil Nil)))) (B ("xyed","xyed-value") Nil Nil)
        ["btmo","gnhq","jttl","kslr","kzfe","mfmv","mvvn","ssja","tydr","xyed"]
        Passed.
        B ("rkun","rkun-value") (B ("hkbo","hkbo-value") (B ("dzth","dzth-value") Nil Nil) (B ("lmgc","lmgc-value") (B ("kmmn","kmmn-value") Nil Nil) (B ("prij","prij-value") Nil Nil))) (B ("ytvo","ytvo-value") (B ("wzga","wzga-value") (B ("uojt","uojt-value") Nil Nil) (B ("xiqn","xiqn-value") Nil Nil)) Nil)
        ["dzth","hkbo","kmmn","lmgc","prij","rkun","uojt","wzga","xiqn","ytvo"]
        Passed.
        B ("aldb","aldb-value") Nil (B ("lqal","lqal-value") (B ("iuou","iuou-value") (B ("cyxs","cyxs-value") Nil Nil) (B ("lpfe","lpfe-value") Nil Nil)) (B ("vhtz","vhtz-value") (B ("rvsg","rvsg-value") (B ("qibl","qibl-value") Nil (B ("qyam","qyam-value") Nil Nil)) Nil) (B ("znat","znat-value") Nil Nil)))
        ["aldb","cyxs","iuou","lpfe","lqal","qibl","qyam","rvsg","vhtz","znat"]
        Passed.
        B ("qgko","qgko-value") (B ("osxr","osxr-value") (B ("lpin","lpin-value") (B ("hhjn","hhjn-value") Nil Nil) (B ("ohtc","ohtc-value") Nil Nil)) Nil) (B ("qqhn","qqhn-value") Nil (B ("snkb","snkb-value") (B ("rzio","rzio-value") (B ("qwuq","qwuq-value") Nil Nil) Nil) (B ("yxfe","yxfe-value") Nil Nil)))
        ["hhjn","lpin","ohtc","osxr","qgko","qqhn","qwuq","rzio","snkb","yxfe"]
        Passed.
        B ("hunp","hunp-value") (B ("aodx","aodx-value") (B ("aftw","aftw-value") Nil Nil) (B ("cchm","cchm-value") Nil (B ("hfsy","hfsy-value") (B ("eykj","eykj-value") (B ("cell","cell-value") Nil Nil) Nil) Nil))) (B ("omzi","omzi-value") (B ("nsns","nsns-value") Nil (B ("obvt","obvt-value") Nil Nil)) Nil)
        ["aftw","aodx","cchm","cell","eykj","hfsy","hunp","nsns","obvt","omzi"]
        Passed.
        B ("qahk","qahk-value") (B ("lwsj","lwsj-value") (B ("amzg","amzg-value") Nil Nil) Nil) (B ("qsxw","qsxw-value") Nil (B ("tlee","tlee-value") (B ("rmxq","rmxq-value") Nil (B ("spbu","spbu-value") Nil Nil)) (B ("yxtb","yxtb-value") (B ("yflg","yflg-value") (B ("vkju","vkju-value") Nil Nil) Nil) Nil)))
        ["amzg","lwsj","qahk","qsxw","rmxq","spbu","tlee","vkju","yflg","yxtb"]
        Passed.
        B ("xjwu","xjwu-value") (B ("gmls","gmls-value") (B ("fsmc","fsmc-value") (B ("chle","chle-value") Nil (B ("fdpz","fdpz-value") Nil Nil)) Nil) (B ("mxrv","mxrv-value") Nil (B ("vpdi","vpdi-value") (B ("rxqc","rxqc-value") Nil (B ("tetg","tetg-value") Nil Nil)) Nil))) (B ("zfoo","zfoo-value") Nil Nil)
        ["chle","fdpz","fsmc","gmls","mxrv","rxqc","tetg","vpdi","xjwu","zfoo"]
        Passed.
        Ran 10 successful.
        (0.02 secs, 2583176 bytes)

    NEXT:: added assert2 properties to insertKeyA: lengthProp, sortedProp, noDupesProp, lookupProp
                

    NEXT:: implemented pretty print:
        Some test results:

                                                                   : -value                                                                  
                                                                      |                                                                      
 ----------------------------------------------------------------------                                                                      
/                                                                      \                                                                     
-                                                                  u:u-value                                                                 
                                                                       |                                                                     
                                                      ---------------------------------------------------------------------                  
                                                     /                                                                     \                 
                                              rgkl:rgkl-value                                                       vhnl:vhnl-value          
                                                     |                                                                     |                 
                                              ---------------------------------------------------                -----------------           
                                             /                                                   \              /                 \          
                                         n:n-value                                          tv:tv-value    um:um-value       zv:zv-value     
                                             |                                                   |              |                 |          
                                  -------------------------------------------                    -------        --               ----------- 
                                 /                                           \                  /       \      /  \             /           \
                          g gu:g gu-value                             oilt:oilt-value      tg:tg-value  -      -  -         w:w-value       -
                                 |                                           |                  |                               |            
                  -------------------------------                            -----------        --                       ---------           
                 /                               \                          /           \      /  \                     /         \          
            d :d -value                   geeg:geeg-value               o:o-value       -      -  -                     -  wkzo:wkzo-value   
                 |                               |                          |                                                     |          
          --------------                  ---------------                  ----------                                             --         
         /              \                /               \                /          \                                           /  \        
    j : j -value  etg:etg-value   gdpp:gdpp-value  lgu:lgu-value   nhxf:nhxf-value   -                                           -  -        
         |              |                |               |                |                                                                  
         --        -------               --              -------    --------                                                                 
        /  \      /       \             /  \            /       \  /        \                                                                
        -  -      -  f :f -value        -  -       kf:kf-value  -  -  nyk:nyk-value                                                          
                          |                             |                   |                                                                
                          --                            --                  --                                                               
                         /  \                          /  \                /  \                                                              
                         -  -                          -  -                -  -                                                              

[" "," j ","d ","etg","f ","g gu","gdpp","geeg","kf","lgu","n","nhxf","nyk","o","oilt","rgkl","tg","tv","u","um","vhnl","w","wkzo","zv"]
Passed.
Ran 3 successful.


Tests with 1 char: (so only a-z)


                                         z:z-value                                         
                                             |                                             
                                            ---------------------------------------------- 
                                           /                                              \
                                       a:a-value                                          -
                                           |                                               
     --------------------------------------------                                          
    /                                            \                                         
 : -value                                    p:p-value                                     
    |                                            |                                         
    --                                ---------------------------------------              
   /  \                              /                                       \             
   -  -                          m:m-value                               x:x-value         
                                     |                                       |             
                               ---------------------------                  -----------    
                              /                           \                /           \   
                          l:l-value                   n:n-value        u:u-value       -   
                              |                           |                |               
                              --------------------    ------               ---------       
                             /                    \  /      \             /         \      
                         d:d-value                -  -  o:o-value     t:t-value     -      
                             |                              |             |                
                 -------------------                        --           --------          
                /                   \                      /  \         /        \         
            c:c-value           h:h-value                  -  -     r:r-value    -         
                |                   |                                   |                  
                ------         ------------                         ------                 
               /      \       /            \                       /      \                
           b:b-value  -   f:f-value    i:i-value                   -  s:s-value            
               |              |            |                              |                
               --         ------           --                             --               
              /  \       /      \         /  \                           /  \              
              -  -       -  g:g-value     -  -                           -  -              
                                |                                                          
                                --                                                         
                               /  \                                                        
                               -  -                                                        

[" ","a","b","c","d","f","g","h","i","l","m","n","o","p","r","s","t","u","x","z"]
Passed.

                                 l:l-value                                  
                                     |                                      
                 --------------------------------------                     
                /                                      \                    
            f:f-value                              o:o-value                
                |                                      |                    
          -----------------              --------------------               
         /                 \            /                    \              
     c:c-value         i:i-value    n:n-value            u:u-value          
         |                 |            |                    |              
     ----------        ------           --           ---------------        
    /          \      /      \         /  \         /               \       
a:a-value  e:e-value  -  k:k-value     -  -     s:s-value       x:x-value   
    |          |             |                      |               |       
    --         --            --                     ------         -------- 
   /  \       /  \          /  \                   /      \       /        \
   -  -       -  -          -  -               q:q-value  -   w:w-value    -
                                                   |              |         
                                                   --             ------    
                                                  /  \           /      \   
                                                  -  -       v:v-value  -   
                                                                 |          
                                                                 --         
                                                                /  \        
                                                                -  -        

["a","c","e","f","i","k","l","n","o","q","s","u","v","w","x"]
Passed.

                                    z:z-value                                     
                                        |                                         
                                        ----------------------------------------- 
                                       /                                         \
                                   k:k-value                                     -
                                       |                                          
                    ----------------------------------------                      
                   /                                        \                     
               j:j-value                                u:u-value                 
                   |                                        |                     
                   --------------------               ------------------          
                  /                    \             /                  \         
              e:e-value                -         m:m-value          x:x-value     
                  |                                  |                  |         
            ------------------                 -----------              ------    
           /                  \               /           \            /      \   
       b:b-value          i:i-value       l:l-value   r:r-value    v:v-value  -   
           |                  |               |           |            |          
     -----------              ------          --          ------       --         
    /           \            /      \        /  \        /      \     /  \        
a:a-value   c:c-value    f:f-value  -        -  -    o:o-value  -     -  -        
    |           |            |                           |                        
    --      ------           --                          --                       
   /  \    /      \         /  \                        /  \                      
   -  -    -  d:d-value     -  -                        -  -                      
                  |                                                               
                  --                                                              
                 /  \                                                             
                 -  -                                                             

["a","b","c","d","e","f","i","j","k","l","m","o","r","u","v","x","z"]

--}

{-- Helper functions below --}

{-- pretty tree print function --}

toTree :: BinTree ([Char], [Char]) -> T.Tree [Char]
toTree Nil = T.Node "-" []
toTree (B (k,d) t1 t2) = T.Node (k ++ ":" ++ d) [toTree t1,toTree t2]

showtree :: BinTree ([Char], [Char]) -> String
showtree = drawVerticalTree.toTree

printtree :: BinTree ([Char], [Char]) -> IO ()
printtree = putStrLn.('\n':).showtree

{-- Functions to generate random Strings [a..z] --}

atoZ = [ x | x <- ['a'..'z']++[' '] ]
--atoZ = [ x | x <- ['a'] ]

getRandomString :: IO String
getRandomString = do 
    n <- getRandomInt 1 4
    s <- getRandomStringOfSize n
    return (s)

getRandomStringOfSize :: Int -> IO String
getRandomStringOfSize 0 = return []
getRandomStringOfSize n = do
       x <- getRandomChar
       xs <- getRandomStringOfSize (n-1)
       return (x:xs)

getRandomChar :: IO Char
getRandomChar = do
    index <- getRandomInt 0 ((length atoZ)-1)
    return (atoZ !! index)

getRandomInt :: Random a => a -> a -> IO a
getRandomInt n m = getStdRandom (randomR (n,m))

{-- Thank you for learning me Haskell. --}