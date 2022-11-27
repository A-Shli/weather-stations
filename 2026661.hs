--
-- MATHFUN
-- Haskell coursework
-- UP2026661
--

--
-- Imports
import Data.Char
import Data.List
import Control.Concurrent (threadDelay)
import Text.Printf
import System.IO

--
-- Types (define your Station type here)

type Name = String
type Coords = (Float, Float)
type Temp = [Float]

type Station = (Name, Coords, Temp)

data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Eq, Show, Ord, Read, Enum)

-- type Database = [Station]

testData :: [Station]
testData = [("Mumbles Head", (51.565, -3.981), [8.26, 8.33, 9.84, 12.36, 15.24, 17.83, 19.55, 19.67, 17.97, 14.70, 11.49, 9.09]),
            ("Greenwich Park", (51.477, 0.004), [8.47, 9.21, 12.07, 15.35, 18.59, 21.37, 23.75, 23.31, 20.29, 15.83, 11.55, 8.85]),
            ("Solent", (50.807, -1.208), [8.56, 8.74, 11.01, 13.94, 17.07, 19.59, 21.62, 21.61, 19.38, 15.73, 11.88, 9.17]),
            ("Ronaldsway", (54.085, -4.632), [8.47, 8.35, 9.44, 11.48, 14.33, 16.52, 18.19, 18.15, 16.56, 13.83, 11.10, 9.17]),
            ("Baltasound", (60.749, -0.850), [6.55, 6.32, 7.35, 9.16, 11.20, 13.25, 15.08, 15.39, 13.62, 10.88, 8.47, 7.00]),
            ("St Austell", (50.337, -4.787), [9.46, 9.65, 11.33, 13.30, 16.18, 18.10, 20.60, 20.36, 18.54, 14.99, 12.30, 10.18]),
            ("Heathrow", (51.479, -0.449), [8.42, 8.98, 11.73, 15.00, 18.37, 21.57, 23.89, 23.40, 20.22, 15.81, 11.47, 8.79]),
            ("Hunstanton", (52.939, 0.493), [7.05, 7.45, 9.77, 12.65, 15.96, 18.84, 21.34, 21.28, 18.32, 14.46, 10.29, 7.56]),
            ("Durham", (54.767, -1.583), [6.86, 7.75, 9.87, 12.49, 15.42, 17.96, 20.24, 19.87, 17.36, 13.51, 9.65, 7.07]),
            ("Monks Wood", (52.400, -0.233), [7.58, 8.36, 11.05, 14.14, 17.19, 20.01, 22.63, 22.49, 19.50, 15.18, 10.68, 7.85 ])]

--
--  Your functional code goes here
--

-- i returns stations of the names of all stations
getNames :: [Station] -> [Name]
getNames  stations = [name | (name, _, _) <- stations]

outputNames :: [Station] -> String
outputNames [] = []
outputNames stations = concat [name ++ " , " | (name, _, _) <- stations]

-- ii add new station to database, given a name, coordinates(N & E) and temperature(12)
addStation :: [Station] -> [Station]
addStation (station:stations) = station:testData

-- iii return all the data with all the temperature values converted to degrees Fahrenheit
celciusToFahrenheit :: [Station] -> [Station]
celciusToFahrenheit stations = [(name, coords, map (\stations -> stations * (9/5) + 32) temperature) | (name, coords, temperature) <- stations]

-- iv given a month and a Celsius value, return a stations of the names of all weather stations
-- that have a higher temperature value for that month
-- stationsForMonth :: [Station] -> [Name]
-- stationsForMonth x = [name | (name, coords, temperature) <- x, temperature !! 7 > 20] -- No input

stationForMonthInp :: Month -> Float -> [Station] -> [Name]
stationForMonthInp month temp stations = [name | (name, coords, temperature) <- stations, temperature !! (fromEnum month) > temp]

{-  v. Return all the data as a single string which, when output using putStr, will display the data formatted neatly
    into a table of 15 columns giving the name, location (degrees N & E formatted to 1 decimal place), and the 12
    temperature values (from January to December each formatted to 1 decimal place)
    printf -}

tempFormat :: [Float] -> String
tempFormat [] = ""
tempFormat (temperature:temps) = printf "%4.1f" temperature ++ " | " ++ tempFormat temps

stationToString :: Station -> String
stationToString (name, coords, temperature) = printf "%15s | %4.1f | %4.1f | %v \n"
    name north east formatted 
    where north = fst coords
          east = snd coords
          formatted = tempFormat temperature

header :: [Station] -> String
header stations = "\tStation\t|  N   |  E   | Jan  | Feb  | Mar  | Apr  | May  | June | July | Aug  | Sept | Oct  | Nov  | Dec  |\n" ++
         "----------------|------|------|------|------|------|------|------|------|------|------|------|------|------|------|\n" ++
        formatStations stations

formatStations :: [Station] -> String
formatStations [] = ""
formatStations stations = foldl (\st stations -> st ++ "" ++ stationToString stations) "" stations

{-  vi. Replace a temperature value given a station name, a month and a new temperature
    Use drop and take to remove the old temperature and replace it with the new one -}

tempIndex :: Name -> String -> Int -> Float -> [Float] -> [Float]
tempIndex stationName name ind temperature temps
        | stationName == name = take ind temps ++ [temperature] ++ drop (ind + 1) temps
        | otherwise = temps

tempUpdate :: Name -> Month -> Float -> [Station] -> [Station]
tempUpdate name month newTemp stations = [(stationName, coords, tempIndex stationName name (fromEnum month) newTemp temperature) | (stationName, coords, temperature) <- stations]

{-  vii. Given a location (N and E figures), a month and a temperature value, return the name of the closest weather 
    station with an higher temperature for that month, or “none” if no such station exists; use Pythagoras’ theorem to
    calculate the distance between locations (i.e. assume the world is flat!) -}

closestStation :: [(Name, Float)] -> Name
closestStation [] = "none"
closestStation [(stationName, coords)] = stationName
closestStation ((stationName, coords):(stationName1, coords1):xs)
 | coords > coords1 = closestStation ((stationName1, coords1):xs)
 | coords < coords1 = closestStation ((stationName, coords):xs)
 | coords == coords1 = closestStation ((stationName1, coords1):xs)

calculateDistance :: Coords -> [Station] -> Month -> Float -> [(Name, Float)]
calculateDistance (x1, y1) stations month temperature' =[(stationName, pythag (x1 ,y1) (x2, y2))
    | (stationName, (x2, y2), temperature) <- stations , temperature !! fromEnum month > temperature']
    where
    pythag (x1 , y1) (x2 , y2) = sqrt(x' * x' + y' * y')
        where
            x' = x1 - x2
            y' = y1 - y2

nearestStation :: Coords -> Month -> Float -> [Station] -> String
nearestStation coords month temperature stations = closestStation (calculateDistance coords stations month temperature)

--
--  Demo
--

demo :: Int -> IO ()
-- output the names of all the weather stations
demo 1 = print (getNames testData)

-- output the data after adding a new station "Valley" with coordinates (53.252, -4.537) and
-- temperature data 8.37, 8.44, 9.84, 12.09, 15.01, 17.24, 18.77, 18.76, 17.26, 14.31, 11.26, 9.09]
demo 2 = putStrLn(header(addStation [("Valley", (53.252, -4.537), [8.37, 8.44, 9.84, 12.09, 15.01, 17.24, 18.77, 18.76, 17.26, 14.31, 11.26, 9.09])]))

-- output the data with all temperature values converted to degrees Fahrenheit
demo 3 = putStrLn (header(celciusToFahrenheit testData))

-- output the names of weather stations with August temperature warmer than 20 degrees Celsius
demo 4 = print (stationForMonthInp August 20 testData)

demo 5 = putStrLn (header testData)

-- output the data after changing the temperature of "Heathrow" for July to 25 degrees Celsius (Lat: 51.470020 Long: -0.454295)
demo 6 = putStrLn(header(tempUpdate "Heathrow" July 25 testData))

-- output the name of the nearest weather station to location (50.2, -0.4)
-- which has a March temperature warmer than 10 degrees Celsius
demo 7 = print ( nearestStation (50.2, -0.4) March 10 testData)
-- animated chart
demo 8 = animatedBarChart testData
--
-- Screen Utilities (use these to do the bar chart)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text

--
-- Your bar chart code goes here
--

-- Returns all the temperatures from stations for that month
getTemps :: [Station] -> Month -> [Float]
getTemps [] month = []
getTemps ((name, coords, temperature):xs) month = temperature !! fromEnum month : getTemps xs month

-- Takes the lists of stations n moths and returns it for the whole year
allTempMonths ::[Station] -> [Month] -> [[(String, Float)]]
allTempMonths stations = map (zip (getNames stations) . getTemps stations)

-- Returns a bar chart showing the temperature in #s.
barChart :: [(String, Float)] -> String
barChart [] = ""
barChart ((name, temperature):xs) = name ++ " " ++ replicate (round (temperature * 2)) '#' ++ "\n" ++ barChart xs

-- Loops through every item and displays the chart)
loopDisplay :: [Station] -> [Month] -> IO ()
loopDisplay [] [] = return ()
loopDisplay _ [] = return ()
loopDisplay [] _ = return ()
loopDisplay stations (month:months) = do
    clearScreen
    putStrLn ("Stations:     " ++ (show month :: String) ++ " degrees Celsius")
    putStrLn (barChart (head (allTempMonths stations [month])))
    threadDelay 1000000
    loopDisplay stations months

-- This animates the bar chart
animatedBarChart :: [Station] -> IO ()
animatedBarChart stations = do
    let months = [January .. December]
    let stationTemps = allTempMonths stations months
    loopDisplay stations months

--
-- Your user interface (and loading/saving) code goes here
--

-- save code
save :: [Station] -> IO ()
save stationList = do
    writeFile "stations.txt"  ("[" ++ saveConversion stationList ++ "]")

saveConversion :: [Station] -> String
saveConversion [] = ""
saveConversion (station:stations) = do
    if length (stations) > 0 then
        stringConversion station ++ "," ++ saveConversion stations
    else
        stringConversion station ++ saveConversion stations

stringConversion :: Station -> String
stringConversion (name, coords, temps) =
    "(\"" ++ name ++ "\", " ++ show coords ++ ", " ++ show temps ++ ")"

readsStation :: IO [Station]
readsStation = do
    contents <- readFile "stations.txt"
    return (read contents)

menuOptions :: [Station] -> IO ()
menuOptions stationList = do
    putStrLn "1. List all stations"
    putStrLn "2. Add a new station"
    putStrLn "3. Convert temperatures to Fahrenheit"
    putStrLn "4. List stations with a temperature warmer than a given value"
    putStrLn "5. Prints stations neatly"
    putStrLn "6. Change a temperature for a station"
    putStrLn "7. Find the nearest station to a given location"
    putStrLn "8. Display a bar chart of the temperature figures"
    putStrLn "9. Quit and save"
    putStrLn "Enter your choice: "
    option <- getLine
    listOfOptions option stationList

listOfOptions :: String -> [Station] -> IO ()
listOfOptions "1" stationList = do
    putStrLn (outputNames stationList)
    threadDelay 1000000
    menuOptions stationList
listOfOptions "2" stationList = do
    putStrLn "Enter the name of the station: "
    stationName <- getLine
    putStrLn "Enter the latitude of the station: "
    stationLat <- getLine
    putStrLn "Enter the longitude of the station: "
    stationLong <- getLine
    putStrLn "Enter the 12 temperatures for the station with spaces in between each temperature: "
    stationTemp <- getLine
    let station = (stationName, (read stationLat :: Float, read stationLong :: Float), (map read (words stationTemp) :: [Float]))
    let newData = addStation [station]
    putStrLn (header newData)
    threadDelay 1000000
    menuOptions newData
listOfOptions "3" stationList = do
    putStrLn "Converted temperature: "
    putStrLn (header(celciusToFahrenheit stationList))
    threadDelay 1000000
    menuOptions stationList
listOfOptions "4" stationList = do
    putStrLn "Enter the month as a word starting with a capital letter, e.g. July: "
    month <- getLine
    putStrLn "Enter the temperature: "
    temperature <- getLine
    putStrLn (unwords (stationForMonthInp (read month :: Month) (read temperature :: Float) stationList))
    threadDelay 1000000
    menuOptions stationList
listOfOptions "5" stationList = do
    putStrLn "Stations formatted neatly: "
    putStrLn (header stationList)
    threadDelay 1000000
    menuOptions stationList
listOfOptions "6" stationList = do
    putStrLn "Enter the name of the station using the stations below: "
    putStrLn "Mumbles Head ,Greenwich Park, Solent, Ronaldsway, Baltasound, St Austell, Heathrow, Hunstanton, Durham, Monks Wood"
    stationName <- getLine
    putStrLn "Enter the month as a word starting with a capital letter, e.g. July: "
    month <- getLine
    putStrLn "Enter the temperature: "
    temperature <- getLine
    putStrLn (header(tempUpdate stationName (read month :: Month) (read temperature :: Float) stationList))
    threadDelay 1000000
    menuOptions stationList
listOfOptions "7" stationList = do
    putStrLn "Enter the latitude: "
    lat<- getLine
    putStrLn "Enter the longitude: "
    long <- getLine
    putStrLn "Enter the month as a word starting with a capital letter, e.g. July: "
    month <- getLine
    putStrLn "Enter the temperature: "
    temperature <- getLine
    putStrLn "The nearest station is: "
    putStrLn (nearestStation (read lat :: Float, read long :: Float) (read month :: Month) (read temperature :: Float) stationList)
    threadDelay 1000000
    menuOptions stationList
listOfOptions "8" stationList = do
    animatedBarChart stationList
    threadDelay 1000000
    menuOptions stationList
listOfOptions "9" stationList = do
    clearScreen
    save stationList
    putStrLn "Goodbye!"
    return ()
listOfOptions _ stationList = do
    putStrLn "Invalid choice"
    menuOptions stationList

-- Main function 
main :: IO ()
main = do
    stationList <- readsStation
    putStr (outputNames stationList)
    putStrLn "Stations and their temperatures UK in a year:\n"
    menuOptions stationList
