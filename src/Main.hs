-- |
-- Module      :  Main
-- Copyright   :  Mauro Taraborelli 2013
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Divisione a due cifre.

module Main
    (
     main
    )
where

import Data.Monoid (mconcat)

import Options.Applicative

import DivisioneDueCifre


main :: IO ()
main = customExecParser (prefs showHelpOnError) optionsPI >>= ddc

ddcVersion :: String
ddcVersion = "ddc 0.1.0.0 - divisione a due cifre"

ddc :: Options -> IO ()
ddc Version                      = putStrLn ddcVersion
ddc (Options dividendo divisore) = print $ dividendo `div2c` divisore

------------------------------------------------
-- Gestione delle opzioni della linea di comando
------------------------------------------------

data Options = Version
             | Options Int Int

optionsPI :: ParserInfo Options
optionsPI = info (helperIt <*> optionsP)
            ( fullDesc
              <> progDesc "Calcola il risultato delle divisioni a due cifre."
              <> header ddcVersion)

optionsP :: Parser Options
optionsP = flag' Version
                   ( long "versione"
                     <> short 'v'
                     <> help "Mostra informazioni sulla versione"
                     <> hidden )
           <|> ( Options
                 <$> argument auto (metavar "DIVIDENDO")
                 <*> argument auto (metavar "DIVISORE"))

-- Gestione helper in italiano
helperIt :: Parser (a -> a)
helperIt = abortOption ShowHelpText $ mconcat
            [ long "help"
            , short 'h'
            , help "Mostra questo aiuto" ]
