import Network.CGI
import Graphics.WordCloud
import Graphics.GD
import Text.XHtml.Strict
import Data.Maybe
import Data.Char
import Data.ByteString.Lazy.Char8 (fromChunks)
import Data.Function
import Control.Monad
import Control.Monad.State.Lazy

main = runCGI (handleErrors main') where
    main' = do
      page <- getInput "p"
      case page of
        Just "render" -> validate renderPage
        _             -> validate viewPage

-- Displays options and a cloud preview.
viewPage :: PageWithInputs
viewPage inputs = output $ showHtml $
  p << image ! [src "?p=render"]

-- Renders the cloud and outputs a PNG image of it.
renderPage :: PageWithInputs
renderPage is | any isInvalid is = redirect "wordclouderror.png"
              where isInvalid (_,_,(Left _)) = True
                    isInvalid _              = False

errorImage :: Page
errorImage = do
  bytes <- liftIO (do img <- newImage (500,500)
                      useFontConfig True
                      fillImage (rgb 50 50 50) img
                      drawErrorString "Invalid input(s)." img
                      savePngByteString img)
  setHeader "" "image/png"
  outputFPS $ fromChunks [bytes]

drawErrorString t =
    drawString "DejaVu Sans" 20 0 (20,40) ("Error: " ++ t) (rgb 180 180 180)

-- Page validator
-- 
validate :: PageWithInputs -> PagePreprocess
validate = (getValidInputs inputs >>=) where
    inputs = [inputR "t" "Cloud text" validText
             ,inputR "mw" "Maximum words" validWords]

type PagePreprocess = Page
type PageWithInputs = [CGIInput] -> Page
type Page = CGI CGIResult

-- Validators
--

maxLen = 10000
maxWords = 150

-- Validates the text used to create the word cloud.
validText :: InputValidator
validText t | tooLong   = Left  $ "Longer than " ++ show maxLen ++ " chars."
            | otherwise = Right $ t
            where tooLong = t `exceeds` maxLen

-- Validates the maximum number of words for the cloud.
validWords :: InputValidator
validWords t | null t    = Left  $ "No number provided!"
             | notNumber = Left  $ "Not a number."
             | tooLarge  = Left  $ "Too large, greater than " ++ show maxWords
             | otherwise = Right $ t
             where notNumber = any (not . isDigit) t
                   tooLarge = t `exceeds` length (show maxWords) || n < maxWords
                   n = read t

-- Validator helper
exceeds :: [a] -> Int -> Bool
exceeds l n = length (take (n+1) l) > maxLen

-- Validation code
--

getValidInputs :: [InputRequest] -> CGI [CGIInput]
getValidInputs = mapM tryValidate where
    tryValidate (name,desc,validate) = do 
      value <- liftM (fromMaybe "") $ getInput name
      return (name,desc,validate value)

inputR :: String -> String -> InputValidator -> InputRequest
inputR = (,,)

type InputRequest = (Name,Desc,InputValidator)
type InputValidator = Value -> Either Err Value
type CGIInput = (Name,Desc,Either Err Value)
type Name  = String
type Desc  = String
type Value = String
type Err   = String
