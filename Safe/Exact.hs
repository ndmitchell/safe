module Safe.Exact(
    takeExact,
    dropExact,
    splitAtExact
    ) where


-- |
-- > takeExact n xs =
-- >   | n >= 0 && n <= length xs = Just (take n xs)
-- >   | otherwise                = Nothing
takeExact :: Int -> [a] -> Maybe [a]
takeExact 0 [] = Just []
takeExact n [] = Nothing
takeExact n (x:xs)
  | n < 0     = Nothing
  | n == 0    = Just []
  | otherwise = fmap (x:) (takeExact (n - 1) xs)


-- |
-- > dropExact n xs =
-- >   | n >= 0 && n <= length xs = Just (drop n xs)
-- >   | otherwise                = Nothing
dropExact :: Int -> [a] -> Maybe [a]
dropExact 0 [] = Just []
dropExact n [] = Nothing
dropExact n (x:xs)
  | n < 0     = Nothing
  | n == 0    = Just (x:xs)
  | otherwise = dropExact (n - 1) xs


-- |
-- > splitAtExact n xs =
-- >   | n >= 0 && n <= length xs = Just (splitAt n xs)
-- >   | otherwise                = Nothing
splitAtExact :: Int -> [a] -> Maybe ([a], [a])
splitAtExact 0 [] = Just ([], [])
splitAtExact n [] = Nothing
splitAtExact n (x:xs)
  | n < 0     = Nothing
  | n == 0    = Just ([], x:xs)
  | otherwise = do (ys, zs) <- splitAtExact (n - 1) xs
                   return (x:ys, zs)


