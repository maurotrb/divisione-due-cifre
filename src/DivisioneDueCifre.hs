-- |
-- Module      :  DivisioneDueCifre
-- Copyright   :  Mauro Taraborelli 2014
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Divisione a due cifre.

module DivisioneDueCifre
    (
     div2c
    )
where

-- | Struttura dati per i numeri a due cifre
--
-- Rende più facili e chiari i calcoli richiesti dall'algoritmo
data NumeroDueCifre = NDC
    { decina :: Cifra
    , unità  :: Cifra
    } deriving Show

-- Alias di tipo per rendere più esplicite le firme dei metodi
type Numero    = Int
type Cifra     = Int

type Dividendo = Numero
type Divisore  = Numero
type Risultato = Numero
type Resto     = Numero

type CifreDividendo = [Cifra]
type CifreDivisore  = [Cifra]

type DividendoDueCifre = NumeroDueCifre
type DivisoreDueCifre  = NumeroDueCifre


-- | Divisione con divisore a due cifre.
--
div2c :: Dividendo -> Divisore -> (Risultato, Resto)
div2c dividendo divisore = abbassaCifreResidue primoResiduo (primoRisultato, primoResto)
    where
      cifreDivisore                    = dividiInCifre divisore
      cifreDividendo                   = dividiInCifre dividendo
      divisore2c                       = creaDivisoreDueCifre cifreDivisore
      (primoDividendo2c, primoResiduo) = creaDividendoDueCifre cifreDividendo cifreDivisore
      (primoRisultato, primoResto)     = div2cDD primoDividendo2c divisore2c
      --
      -- Completa la divisione, abbassando le cifre residue del dividendo preventivamente
      -- aumentate delle cifre del resto.
      -- Il risultato viene accumulato, il resto è quello dell'ultima divisione.
      abbassaCifreResidue :: CifreDividendo -> (Risultato, Resto) -> (Risultato, Resto)
      abbassaCifreResidue []      rr                 = rr
      abbassaCifreResidue residuo (risultato, resto) = abbassaCifreResidue residuo' (risultato'', resto')
          where
            (dividendo2c, residuo') = creaResiduoDueCifre residuo (dividiInCifre resto)
            (risultato', resto')    = div2cDD dividendo2c divisore2c
            risultato''             = comeDecina risultato + risultato'

-- | Crea il numero a due cifre per il divisore.
--
-- Verifica che il divisore sia esattamente di due cifre.
creaDivisoreDueCifre :: CifreDivisore -> DivisoreDueCifre
creaDivisoreDueCifre (cd1:cd2:[]) = NDC cd1 cd2
creaDivisoreDueCifre _            = error "Il divisore non ha due cifre."


-- | Crea il numero a due cifre per il dividendo.
--
-- Restituisce anche il residuo del dividendo non utilizzato nel numero a due cifre.
-- Da utilizzare per il dividendo completo, dato che controlla se le prime due
-- cifre del dividendo siano sufficienti per il divisore oppure siano necessarie
-- tre cifre.
-- Verifica che il dividendo non sia inferiore al divisore.
creaDividendoDueCifre :: CifreDividendo -> CifreDivisore -> (DividendoDueCifre, CifreDividendo)
creaDividendoDueCifre (cd1:cd2:[])      divisore = if   unisciInNumero [cd1, cd2] >= unisciInNumero divisore
                                                   then (NDC cd1 cd2, [])
                                                   else error "Il dividendo è inferiore al divisore."
creaDividendoDueCifre (cd1:cd2:cd3:cds) divisore = if   unisciInNumero [cd1, cd2] >= unisciInNumero divisore
                                                   then (NDC cd1                         cd2, cd3:cds)
                                                   else (NDC (unisciInNumero [cd1, cd2]) cd3, cds    )
creaDividendoDueCifre _                 _        = error "Il dividendo è inferiore al divisore."


-- | Crea il numero a due cifre per il resto ed il residuo del dividendo.
--
-- Restituisce anche il residuo del dividendo non utilizzato nel numero a due cifre.
-- Verifica che il residuo non sia vuoto.
creaResiduoDueCifre :: CifreDividendo -> CifreDividendo -> (DividendoDueCifre, CifreDividendo)
creaResiduoDueCifre (rd1:rds) res = (NDC (unisciInNumero res) rd1, rds)
creaResiduoDueCifre _         _   = error "Il residuo è vuoto."


-- | Divisione con dividendo e divisore a due cifre
--
-- Formula l'ipotesi iniziale e poi chiama la funzione di cerca risultato.
div2cDD :: DividendoDueCifre -> DivisoreDueCifre -> (Risultato, Resto)
div2cDD dividendo divisore = cercaRisultato ipotesi dividendo divisore
    where
      ipotesi = decina dividendo `div` decina divisore


-- | Cerca il risultato di una divisione con dividendo e divisore a due cifre.
--
-- Verifica la validità dell'ipotesi iniziale:
--
--      * se valida restituisce l'ipotesi/risultato ed il resto
--
--      * se non valida continua la ricerca decrementando l'ipotesi
--
-- L'ipotesi è valida se il numero composto dal prestito e dall'unità del dividendo
-- diviso per l'unità del divisore è maggiore o uguale all'ipotesi. Se l'unità del
-- divisore è uguale a zero, il risultato della divisione è considerato infinito
-- e quindi l'ipotesi è considerata valida.
cercaRisultato :: Risultato -> DividendoDueCifre -> DivisoreDueCifre -> (Risultato, Resto)
cercaRisultato (-1)    _         _        = error "L'ipotesi è diventata negativa."
cercaRisultato ipotesi dividendo divisore = let prestito = decina dividendo - ipotesi * decina divisore
                                                numero   = comeDecina prestito + unità dividendo
                                                verifica = if unità divisore == 0
                                                           then 100 -- Infinito
                                                           else numero `div` unità divisore
                                                resto    = numero - ipotesi * unità divisore
                                            in if   verifica >= ipotesi
                                               then (ipotesi, resto)
                                               else cercaRisultato (ipotesi - 1) dividendo divisore


----------------------
-- Funzioni di utilità
----------------------

-- | Divide un numero nelle cifre che lo compongono
dividiInCifre :: Numero -> [Cifra]
dividiInCifre numero = if   numero == 0
                       then [0]
                       else dividiInCifre' numero
    where
      dividiInCifre' 0 = []
      dividiInCifre' x = dividiInCifre' (x `div` 10) ++ [x `mod` 10]


-- | Unisce le cifre per formare un numero
unisciInNumero :: [Cifra] -> Numero
unisciInNumero = foldl (\numero cifra -> comeDecina numero + cifra) 0


-- | Converte il numero in una decina
comeDecina :: Numero -> Numero
comeDecina = (*) 10
