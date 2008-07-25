-- Compile: ghc --make IRCCloud.hs -o irccloud -O2
--

import System
import Graphics.WordCloud
import Graphics.GD

main = do
  args <- getArgs
  case args of
    (log:out:_) -> let hist = histogramByFreq stripWords
                   in readFile log >>= makeCloud config . hist >>= savePngFile out
    _           -> error "Usage: log.txt output.png"

config = def { confBGColor     = (255,255,255)
             , confColor       = (250,200,50) 
             , confCanvasSize  = (800,600)
             , confDefaultPos  = (0,70)
             , confFontSizeMin = 10
             , confFontSize    = 72
             , confMaxWords    = 100
}--             , confCloudAlgo   = Circular }

stripWords = boringWords ++ words "join joined quit Connection closed connection ubuntu"