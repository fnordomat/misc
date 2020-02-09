-- build: ghc --make -O3 GzipViz -main-is GzipViz.main -o gzipviz
-- This program is a rough draft. See [BUG] below for security advisory

{-# LANGUAGE ParallelListComp, ScopedTypeVariables #-}

module GzipViz where

import Data.List
import Data.Char (chr, ord)
import Data.Maybe (fromJust)
import Data.Bits

import Text.Printf

import System.IO
import System.Process

ztext_example1 :: [Char]
ztext_example1 = "1f8b0800" ++ "5ad32e5d" ++ "0203" ++ "5d92bd6ec3300c84f73ec5ed0d02b428daa163a73e862c3131015934442a7f4f5fca6ed2a48b2148e4dd77a4bf64a65a38729bc08a18726c39182598600c0782d2816ac8986b2f3526858dc19078b7a38a5d15ef3445e6fd687e31ca2459f6cdebb8605fa5cd7879dde0c2256edc204ddd2b9484896a6cf5fc89d4a8db5572673eb01a4790ab47d38d6b630a673885abb103b95ef77b4fa0ec25554a7752a39020bbe5ed432185748bafbb78635084ac82815cea31e92caa3ce4337494a3e723c88993d348819a57e1f96d83e3c899aed84ba9763c8f29c57bdd1251a6595a594812ebdcba019d3c1395484bec3e88e5709d451214b1d5dae57cb621e72dbeed1ff25c2971fc251e1c442a2d6b604f62fdb243f3a553fcc5be2da850b3bec8358fefb0ac41fd536f7bea339da8f4c177442e89dc2f3efe25749ae98ea3ab4ca2e62986653ee62eb2aa7b1f8b53c382bf6d9f7e00" ++ "445d9755" ++ "70020000"

-- Only used to read hexdumped inputs
hex2bin :: Num a => Char -> [a]
hex2bin '0' = [0,0,0,0]
hex2bin '1' = [1,0,0,0]
hex2bin '2' = [0,1,0,0]
hex2bin '3' = [1,1,0,0]
hex2bin '4' = [0,0,1,0]
hex2bin '5' = [1,0,1,0]
hex2bin '6' = [0,1,1,0]
hex2bin '7' = [1,1,1,0]
hex2bin '8' = [0,0,0,1]
hex2bin '9' = [1,0,0,1]
hex2bin 'a' = [0,1,0,1]
hex2bin 'b' = [1,1,0,1]
hex2bin 'c' = [0,0,1,1]
hex2bin 'd' = [1,0,1,1]
hex2bin 'e' = [0,1,1,1]
hex2bin 'f' = [1,1,1,1]
hex2bin 'A' = [0,1,0,1]
hex2bin 'B' = [1,1,0,1]
hex2bin 'C' = [0,0,1,1]
hex2bin 'D' = [1,0,1,1]
hex2bin 'E' = [0,1,1,1]
hex2bin 'F' = [1,1,1,1]
hex2bin x   = error ("not a hex digit: " ++ show x)

char2bin :: Char -> [Int]
char2bin b = [(ord b`shiftR`i) .&. 1 | i <- [0,1,2,3,4,5,6,7]]

-- State of the Decompressor
data State
  -- initial state
  = I0
  | B0 {b1_bfinal::Maybe Int,
        b1_btype::Maybe (Int, Int),
        b3_hlit::Maybe Int,
        b3_hdist::Maybe Int,
        b3_hclen::Maybe Int}
  | ReadLitLen {b3::State, clatree::Tree01 Int, litlen_code_cls::[(Int, Int)], litlen_read_next::Int}
  | ReadDists  {b3::State, clatree::Tree01 Int, llctree::Tree01 LLC, dists_cls::[(Int, Int)], dists_read_next::Int}
  | ReadMain   {b3::State, clatree::Tree01 Int, llctree::Tree01 LLC, disttree::Tree01 Int, decoded_prefix::[DecodedExtent]}
  | BlockDone  {rs::State}
  -- "error" with reason + diagnostics
  | Err String String
  -- "not yet implemented"
  | Nyi String String
    deriving (Show)

isErr :: State -> Bool
isErr (Err _ _) = True
isErr _ = False
isNyi :: State -> Bool
isNyi (Nyi _ _) = True
isNyi _ = False

data DecodedExtent = DE {de_numbits :: Int, de_text :: [Int]}
  deriving (Show)

extents_to_string :: [DecodedExtent] -> [Int]
extents_to_string [] = []
extents_to_string ((DE _ w):t) = w ++ extents_to_string t

data LLC = Literal Int | Stop | Lencode Int
  deriving Show

toLLC :: Int -> LLC
toLLC i | i < 256 = Literal i
toLLC 256 = Stop
toLLC i = Lencode i

readlitlen_hlit :: State -> Int
readlitlen_hlit = fromJust . b3_hlit . b3
readdist_hdist  :: State -> Int
readdist_hdist  = fromJust . b3_hdist . b3

decode :: State -> [Int] -> (State, [Int])
decode s [] = (Err "input too short" (show s), [])
decode s@(Nyi _ _) x = (s, x)
decode s@(Err _ _) x = (s, x)
-- Skip fixed number of bytes. Inaccurate in general, headers may be longer (filename, other extensions)
decode s@I0 bs
  | length bs < 8 * 10 = (Err "header too short" (show s), bs)
  | otherwise = decode (B0 Nothing Nothing Nothing Nothing Nothing) (drop (8 * 10) bs)
decode s@(B0 Nothing Nothing _ _ _) (h:t) = (s { b1_bfinal = Just h }, t)
-- Undefined block type
decode s@(B0 (Just _) Nothing _ _ _) bs@(1:1:_) = (Err "BTYPE 0b11" (show s), bs)
-- fixed-huff block, not yet implemented
decode s@(B0 (Just _) (Just (0,1)) _ _ _) t = (Nyi "BTYPE 0b01" (show s), t)
-- uncompressed block, not yet implemented
decode s@(B0 (Just _) (Just (0,0)) _ _ _) t = (Nyi "BTYPE 0b00" (show s), t)
decode s@(B0 (Just b) (Just (1,0)) Nothing Nothing Nothing) bs =
  case readValue 5 bs of
    Nothing -> (Err "input too short" (show s), bs)
    Just (v0, bs') ->
      case readValue 5 bs' of
        Nothing -> (Err "input too short" (show s), bs')
        Just (v1, bs'') ->
          case readValue 4 bs'' of
            Nothing -> (Err "input too short" (show s), bs'')
            Just (v2, t) ->
              (B0 (Just b) (Just (1,0)) (Just (v0+257)) (Just (v1+1)) (Just (v2+4)), t)
decode s@(B0 (Just _) Nothing _ _ _) (h:i:t) = (s { b1_btype = Just (i,h) }, t)
decode s@(B0 _ _ (Just _) (Just _) (Just hclen')) bs =
  if
    length bs < 3 * hclen' then (Err "input too short" (show s), bs)
  else
    let
      list = (sort (filter ((/=0).fst) (zip [bin2int (take 3 (drop (3*i) bs))| i <- [0..hclen'-1]] [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15])))
      tree = mkTree list
    in
      case tree of
         Nothing    -> (Err "couldn't parse codelength alphabet tree" (show s), bs) 
         Just tree' -> (ReadLitLen s tree' [] 0, drop (3*hclen') bs)

decode s@(ReadLitLen s' theClatree codelengths i) bs
  | i == readlitlen_hlit s =
    case mkTree (map (\(x,y) -> (x,toLLC y)) (sort (filter ((/=0).fst) codelengths))) of
      Nothing -> (Err "couldn't parse literal/length alphabet tree" (show s), bs)
      Just tr -> (ReadDists s' theClatree tr codelengths 0, bs)

decode s@(ReadLitLen s' theClatree codelengths i) bs =
  case readTree theClatree bs of
    Nothing        -> (Err ("couldn't read with" ++ show theClatree) (show s), bs)
    Just (16, bs') -> if codelengths == [] 
                         then (Err "error" (show s), bs)
                         else case readValue 2 bs' of
                          Nothing -> (Err "input too short" (show s), bs)
                          Just (x, bs'') -> (ReadLitLen s' theClatree ([ ((fst.head $ codelengths), i+counter) | counter<-[0..3+x-1] ]++codelengths) (i+3+x), bs'')
    Just (17, bs') -> case readValue 3 bs' of
                          Nothing -> (Err "input too short" (show s), bs)
                          Just (x, bs'') -> (ReadLitLen s' theClatree ([ (0, i+counter) | counter<-[0..3+x-1] ]++codelengths) (i+3+x), bs'')
    Just (18, bs') -> case readValue 7 bs' of
                          Nothing -> (Err "input too short" (show s), bs)
                          Just (x, bs'') -> (ReadLitLen s' theClatree ([ (0, i+counter) | counter<-[0..11+x-1] ]++codelengths) (i+11+x), bs'')
    Just (v,  bs') -> (ReadLitLen s' theClatree ((v,i):codelengths) (i+1), bs')

decode s@(ReadDists s' theClatree theTheLlctree dists i) bs
  | i == readdist_hdist s =
    case mkTree (map (\(x,y) -> (x, y)) (sort (filter ((/=0).fst) (take (fromJust . b3_hdist . b3 $ s) dists)))) of
      Nothing -> (Err "couldn't parse hdist tree" (show s), bs)
      Just tr -> (ReadMain s' theClatree theTheLlctree tr [], bs)

decode s@(ReadDists s' theClatree theLlctree dists i) bs =
  case readTree theClatree bs of
    Nothing        -> (Err "couldn't read input" (show s), bs)
    Just (16, bs') -> if dists == [] 
                         then (Err "no dists" (show s), bs')
                         else case readValue 2 bs' of
                          Nothing -> (Err "input too short" (show s), bs')
                          Just (x, bs'') -> (ReadDists s' theClatree theLlctree ([ ((fst.head $ dists), i+counter) | counter<-[0..3+x-1] ]++dists) (i+3+x), bs'')
    Just (17, bs') -> case readValue 3 bs' of
                          Nothing -> (Err "input too short" (show s), bs')
                          Just (x, bs'') -> (ReadDists s' theClatree theLlctree ([ (0, i+counter) | counter<-[0..3+x-1] ]++dists) (i+3+x), bs'')
    Just (18, bs') -> case readValue 7 bs' of
                          Nothing -> (Err "input too short" (show s), bs')
                          Just (x, bs'') -> (ReadDists s' theClatree theLlctree ([ (0, i+counter) | counter<-[0..11+x-1] ]++dists) (i+11+x), bs'')
    Just (v,  bs') -> (ReadDists s' theClatree theLlctree ((v,i):dists) (i+1), bs')

-- TODO use a monad to consume the input stream - and for error handling

decode s@(ReadMain _ _ theLlctree theDisttree theDecodedPrefix) bs =
  case readTree theLlctree bs of
    Nothing ->                (Err "couldn't read literal/length code" (show s), bs)
    Just (Literal c, bs') -> (s { decoded_prefix = DE (length bs - length bs') [c]:theDecodedPrefix }, bs')

    Just (Stop, bs')      ->  (BlockDone s, bs')
    Just (Lencode l, bs')  ->
       case readValue (lxbits l) bs' of
         Just (lx, bs'')  ->
           let len = lenbase l + lx
           in  case readTree theDisttree bs'' of
              Nothing -> (Err "coudln't read distance code" (show s), bs')
              Just (d, bs''') -> case readValue (distbits d) bs''' of
                                  Just (dx, bs'''') ->
                                     let dis = distbase d + dx
                                         relevant_rsuffix :: [Int] = take dis
                                           (extents_to_string theDecodedPrefix)
                                         (rx,sx) = divMod len (length relevant_rsuffix)
                                         w :: [Int] = drop (length relevant_rsuffix - sx) relevant_rsuffix ++ concat [relevant_rsuffix | _ <- [0..rx-1]]
                                     in
                                       (s { decoded_prefix = ((DE (length bs - length bs'''') w) : theDecodedPrefix) }, bs'''')
                                  Nothing -> (Err "input too short, couldn't read distance extra bits" (show s), bs''')
         Nothing -> (Err "input too short, couldn't read length extra bits" (show s), bs')

-- decode s$(BlockDone s, bs) = (BlockDone s, bs) -- todo: allow multi-block streams
decode s bs = (Nyi "catch-all" (show s), bs)

distbits :: Int -> Int
distbits d | d < 4 = 0
distbits d | d < 6 = 1
distbits d | d < 8 = 2
distbits d | d < 10 = 3
distbits d | d < 12 = 4
distbits d | d < 14 = 5
distbits d | d < 16 = 6
distbits d | d < 18 = 7
distbits d | d < 20 = 8
distbits d | d < 22 = 9
distbits d | d < 24 = 10
distbits d | d < 26 = 11
distbits d | d < 28 = 12
distbits 28 = 13
distbits 29 = 13
distbits _ = error "no such distance extra bits code"

distbase :: Int -> Int
distbase d | d < 5 = d + 1
distbase 4 = 5
distbase 5 = 7
distbase 6 = 9
distbase 7 = 13
distbase 8 = 17
distbase 9 = 25
distbase 10 = 33
distbase 11 = 49
distbase 12 = 65
distbase 13 = 97
distbase 14 = 129
distbase 15 = 193
distbase 16 = 257
distbase 17 = 385
distbase 18 = 513
distbase 19 = 769
distbase 20 = 1025
distbase 21 = 1537
distbase 22 = 2049
distbase 23 = 3073
distbase 24 = 4097
distbase 25 = 6145
distbase 26 = 8193
distbase 27 = 12289
distbase 28 = 16385
distbase 29 = 24577
distbase _ = error "no such distance code"

lxbits :: Int -> Int
lxbits l | l < 265 = 0
lxbits l | l < 269 = 1
lxbits l | l < 273 = 2
lxbits l | l < 277 = 3
lxbits l | l < 281 = 4
lxbits l | l < 285 = 5
lxbits 285 = 0
lxbits _ = error "no such length extra bits code"

lenbase :: Int -> Int
lenbase l | l < 265 = l - 257 + 3
lenbase 265 = 11
lenbase 266 = 13
lenbase 267 = 15
lenbase 268 = 17
lenbase 269 = 19
lenbase 270 = 23
lenbase 271 = 27
lenbase 272 = 31
lenbase 273 = 35
lenbase 274 = 43
lenbase 275 = 51
lenbase 276 = 59
lenbase 277 = 67
lenbase 278 = 83
lenbase 279 = 99
lenbase 280 = 115
lenbase 281 = 131
lenbase 282 = 163
lenbase 283 = 195
lenbase 284 = 227
lenbase 285 = 258
lenbase _ = error "no such length code"

data Tree01 a =  Tip a | Tree {t0::Tree01 a, t1::Tree01 a}
  deriving Show

-- @precondition: ls ascending by fst!!
mkTree :: [(Int, a)] -> Maybe (Tree01 a)
mkTree ls =
  case mkTree' ls of
     (t, []) -> Just t
     _       -> Nothing

mkTree' :: [(Int, a)] -> (Tree01 a, [(Int, a)])
mkTree' ((0,a):rest) = (Tip a, rest)
mkTree' ls           = (Tree left_hand_branch right_hand_branch, map (\(i,a) -> (i+1,a))  leftovers)
  where
    (left_hand_branch, rest_l)     = mkTree' (map (\(i,a) -> (i-1,a)) ls)
    (right_hand_branch, leftovers) = mkTree' rest_l

readValue nbits bs | nbits > length bs = Nothing
readValue nbits bs = Just (bin2int (take nbits bs), drop nbits bs)

readTree (Tip a) bs    = Just (a, bs)
readTree (Tree _ _) [] = Nothing
readTree (Tree l _) (0:t) = readTree l t
readTree (Tree _ r) (1:t) = readTree r t

-- LSB first:
bin2int :: [Int] -> Int
bin2int bs = sum [b * 2^i | b <- bs | i <- [0::Int ..]]

swaps :: [a] -> [a]
swaps (h:i:t) = i:h:swaps t
swaps x = x

-- Limitations:
--   * can't parse headers, extensions such as filename must not be present (a fixed number of bytes is skipped)
--   * can only parse one BTYPE=dynamic-huffman block, nothing else
--     (no consecutive blocks either: only one block, upto 32k of uncompressed data)
--   * [BUG]: mkTree isn't robust against broken inputs (!!!)
--     may loop indefinitely. This vulnerability must be fixed.
--     don't use this as a CGI script just yet ... ;-)
--   * visualization doesn't show (size of) headers and Huffman tables
-- output goes into viz.html (truncating existing file if present)
-- input is read from input.txt
main :: IO ()
main = do
   (_, Just ho1, _, hp1) <- createProcess (proc "/bin/gzip" ["-9", "-c", "-n", "input.txt"])  {std_out=CreatePipe}
   hSetEncoding ho1 latin1
   sOut <- hGetContents ho1
   _ <- waitForProcess hp1
   -- read bytes directly instead of hexdumping them first :-)
   let intxt :: [Char] = sOut
   -- concatMap ((\x -> printf "%02x" (x::Int)) . ord) sOut
   putStrLn (printf "gzip Compressed input length %d" (length intxt))

   --  print $ mkTree' [(3,0),(3,4),(3,5),(3,6),(3,7),(3,8),(3,17),(4,3),(5,18),(6,2),(6,16)]
   let (s, _) = last $ takeWhile (\(x,_) -> not (isErr x) && not (isNyi x))
                     $ iterate (uncurry decode) (I0, concatMap char2bin intxt)
   -- concatMap hex2bin (swaps intxt))
   let dec = case s of
               BlockDone s' -> decoded_prefix s'
               ReadMain {}  -> decoded_prefix s
               x            -> error (show x)
   --   print $ show dec -- (decoded_prefix s)
   --   print $ map chr $ reverse (extents_to_string dec) -- (decoded_prefix s))
   putStrLn "done, writing output ..."
   handle <- openFile "viz.html" WriteMode
   hPutStr handle $ extentsToHtml (reverse dec)
   hClose handle
   putStrLn "done."

-- TODO: spaces at the ends of display lines are not colored. How to achieve that in HTML/CSS?
-- TODO more compact output using style definitions
extentsToHtml :: [DecodedExtent] -> String
extentsToHtml extents = "<html><head/><body><style>" ++ sout_style ++ "</style><div>" ++ aux extents ++ "</div></body></html>\n"
  where
    bpcs :: [Double] = map (\(DE b l) -> (fromIntegral b) / (fromIntegral (length l))) extents
    aux [] = []
--    aux ((DE bits ints):t) = "<strike style=\" text-decoration: none; background-image: linear-gradient(transparent " ++ (show $ y bits ints) ++ "px, " ++ (c bits ints) ++ " 1em); \">" ++ concatMap saferUChr (reverse ints) ++ "</strike>" ++ aux t
    aux ((DE bits ints):t) = "<strike style=\" text-decoration: none; background-image: linear-gradient(" ++ (c bits ints) ++ " " ++ (show $ y bits ints) ++ "px, " ++ (c bits ints) ++ " 1em); \">" ++ concatMap saferUChr (reverse ints) ++ "</strike>" ++ aux t
    y _ _ = 0.0::Double
    bps bits ints = (fromIntegral bits) / (fromIntegral (length ints))
    c bits ints = printf "#%02x%02x%02x"
      ((floor $ 255.0 *  (      bps bits ints / maximum bpcs ))::Int)
      ((floor $ 255.0 *  (1.0 - bps bits ints / maximum bpcs ))::Int)
      -- (0::Int)
      (if length ints > 1 then 255::Int else 0::Int)

saferUChr :: Int -> [Char]
saferUChr i | i < 32 = "▒" -- light shade 1/4 block
saferUChr i | chr i == '<' = "&lt;"
saferUChr i | chr i == '>' = "&gt;"
--saferUChr i | chr i == ' ' = " "
saferUChr i = [chr i]
      
-- unused functions
org_aux [] = []
org_aux ((x,y):t) =
 let (l,m) = splitPred' [] ((==x).fst) t
 in  (x,y:map snd l):org_aux m
splitPred' accum p (h:t) | p h = splitPred' (h:accum) p t
splitPred' accum _ ls = (reverse accum, ls)

sout_style =
 "body {   background-color: black;  color:#222;  font-size: 2em;  word-break: break-all;  font-family: monospace, monospace; }  "
