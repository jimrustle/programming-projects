
fahren_to_celsius :: (Fractional a, Num a) => a -> a
fahren_to_celsius temp = (temp-32) * 5.0 / 9.0

celsius_to_fahren :: (Fractional a, Num a) => a -> a
celsius_to_fahren temp = temp * 9.0 / 5.0 + 32

main = do
    putStrLn "Enter a temperature"
    temp <- readLn
    putStrLn $ "Celsius: " ++ (show $ fahren_to_celsius temp)
    putStrLn $ "Fahrenheit: " ++ (show $ celsius_to_fahren temp)
