-- Main.hs
module Main where

import Control.Monad
import Safe (readMay)
import Data.NineP
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Concurrent.Chan
import Control.Arrow ((&&&))

import Details

-- Itse fizzbuzz deggistä mukaillen
fizzB i = (++ "\n") . fromMaybe (show i) $ ("Fizz" `maybeFz` 3) `mappend` ("Buzz" `maybeFz` 5)
    where maybeFz str x = str <$ guard (i `rem` x == 0)

-- Kaksi tiedostotyyppiä
data IOFile = IOFile (Chan Int)
data ConstFile = ConstFile String


instance RW IOFile where
    -- Odotetaan tiedoston kanavasta saapuvaksi kokonaislukua ja palautetaan fizzbuzz-tulkinta
    -- Palvelin käsittelee jokaisen pyynnön omassa säikeessään, odottavia pyyntöjä voi olla liikkeellä n+1
    read' (IOFile chan) offset count = readChan chan >>= return . fizzB

    -- Koetetaan lukea kokonaisluku. Onnistuessa lähetetään kirjoitettavaksi, muutoin ilmoitetaan 
    -- kirjoituksen epäonnistumisesta
    write' (IOFile chan) msg = f (readMay msg)
        where f (Just i) = writeChan chan i >> (return . Just $ length msg)
              f Nothing = return Nothing

instance RW ConstFile where
    -- Palautetaan pyydetty osa tallennetusta tekstistä
    read' (ConstFile txt) offset count = return $ section txt
        where section = take count . drop offset

    -- Kirjoitusyritykset epäonnistukoot
    write' _ _ = return Nothing


main = do 
    -- Alkuun luodaan suodatinta matkiva tiedosto 
    buzzdev <- addFile "fizzbuzz" . IOFile <$> newChan
    
    -- Sitten jokunen numeroilla nimetty tiedosto, jotka sisältävät vastaavan fizzbuzz-arvon
    let constFiles = [addFile (show i) (ConstFile (fizzB i)) | i <- [10..20]]
    
    -- Lopuksi yhdistetään tiedostopuut ja aletaan kuunnella 9P-yhteyksiä
    simpleServer 5556 $ buzzdev : constFiles


