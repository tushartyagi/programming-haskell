import Data.Char

type Bit = Int

bin2int' :: [Bit] -> Int
bin2int' bits = sum [w * b | (w,b) <- zip weights bits]
    where weights = iterate (*2) 1

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode =  concat . map (add_parity . make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . check_parity) . chop9 

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = tail

add_parity :: [Bit] -> [Bit]
add_parity xs | even parity_bits = 0 : xs
              | otherwise = 1 : xs
  where parity_bits = length ones
        ones = filter (\x -> x == 1) xs

check_parity xs = if (head xs == 0 && even data_bits) ||
                     (head xs == 1 && odd  data_bits)
                  then tail xs
                  else error ("Invalid parity found.")
  where data_bits = length (filter ones (tail xs)) 
        ones = (\x -> x == 1)
