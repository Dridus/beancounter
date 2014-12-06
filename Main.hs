module Main (main) where

import           Control.Applicative (Applicative, pure)
import           Control.Category.Unicode ((∘))
import           Control.Monad (Monad, fail, forM, forM_)
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.Unicode ((=≪), (≫=))
import           Data.Conduit (Sink, ($$), (=$=))
import           Data.Conduit.Binary (sourceFile)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import           Data.Either (Either(Left, Right))
import           Data.Eq (Eq)
import           Data.Eq.Unicode ((≢))
import           Data.Fixed (Centi, Micro)
import           Data.Function (($))
import           Data.Functor (fmap)
import           Data.Int (Int)
import           Data.List (sum, zip)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (Maybe(Just, Nothing), maybe)
import           Data.Monoid (Monoid(mappend, mempty))
import           Data.Monoid.Unicode ((⊕))
import           Data.Text (Text, intercalate, pack, splitOn, strip, unpack)
import           Prelude (Fractional, Num, Ord, Real, RealFrac, (+), fromRational, toRational, round)
import           Prelude.Unicode ((÷), (⋅))
import           System.Environment (getArgs)
import           System.IO (IO, putStrLn)
import           Text.Read (readEither)
import           Text.Show (Show, show)

newtype Grams = Grams { unGrams ∷ Centi } deriving (Eq, Fractional, Num, Ord, Real, Show)
instance Monoid Grams where
    mempty = 0
    mappend = (+)

newtype Money = Money { unMoney ∷ Micro } deriving (Eq, Fractional, Num, Ord, Real, Show)
instance Monoid Money where
    mempty = 0
    mappend = (+)

data BagHeader  = BagHeader { bagName ∷ !Text, bagOwner ∷ !Text, bagCost ∷ !Money } deriving (Show)
data Bag        = Bag { bagHeader ∷ !BagHeader, bagDoses ∷ [Dose] } deriving (Show)
data Dose       = Dose { dosePerson ∷ !Text, doseAmount ∷ Grams } deriving (Show)
data PricedBag  = PricedBag { pbagHeader ∷ !BagHeader, pbagSize ∷ !Grams, pbagDoses ∷ [PricedDose] } deriving (Show)
data PricedDose = PricedDose { pdose ∷ !Dose, pdoseCost ∷ !Money } deriving (Show)

main ∷ IO ()
main = do
    args ← getArgs
    bags ← forM args $ \ bag →
        runResourceT $ sourceFile bag =$= CT.decode CT.utf8 =$= CT.lines $$ parseBag

    let pricedBags ∷ [PricedBag] = fmap priceBag bags
    let bagSummaries ∷ [Map Text (Grams, Money)] = fmap summarizeBag pricedBags
    let totalSummary ∷ Map Text (Grams, Money) = Map.unionsWith (⊕) bagSummaries
    let people = List.filter (≢ "--") ∘ Map.keys $ totalSummary
    putStrLn ∘ unpack $ "Bag\tBuyer\tBag Cost\tBag Weight\t" ⊕ intercalate "\t" (people ≫= \ person → [person ⊕ " g", person ⊕ " %", person ⊕ " $"])
    forM_ (pricedBags `zip` bagSummaries) $ \ (PricedBag { pbagHeader = BagHeader { .. }, pbagSize }, bagSummary) →
        let
            toCenti = (fromRational ∘ toRational) ∷ Micro → Centi
            toMicro = (fromRational ∘ toRational) ∷ Centi → Micro

            roundToInt ∷ RealFrac a ⇒ a → Int
            roundToInt = round

            percentageOfBag g = 100 ⋅ (toMicro (unGrams g) ÷ toMicro (unGrams pbagSize))

            peopleColumns = intercalate "\t" $ people ≫= \ person →
                let (g, m) = Map.findWithDefault mempty person bagSummary
                in [pack ∘ show ∘ unGrams $ g, pack ∘ show ∘ roundToInt ∘ percentageOfBag $ g, pack ∘ show ∘ toCenti ∘ unMoney $ m]
        in putStrLn ∘ unpack
            $ bagName                                     ⊕ "\t"
            ⊕ bagOwner                                    ⊕ "\t"
            ⊕ (pack ∘ show ∘ toCenti ∘ unMoney $ bagCost) ⊕ "\t"
            ⊕ (pack ∘ show ∘ unGrams $ pbagSize)          ⊕ "\t"
            ⊕ peopleColumns

    pure ()

{-
printBag ∷ Bag → IO ()
printBag (Bag { bagHeader = BagHeader { .. }, bagDoses }) = do
    putStrLn ∘ unpack $ "Name = " ⊕ bagName ⊕ " owner = " ⊕ bagOwner ⊕ " cost = $" ⊕ (pack ∘ show ∘ unMoney $ bagCost)
    forM_ bagDoses $ \ (Dose { .. }) →
        putStrLn ∘ unpack $ "    " ⊕ dosePerson ⊕ "\t" ⊕ (pack ∘ show ∘ unGrams $ doseAmount)

printPricedBag ∷ PricedBag → IO ()
printPricedBag (PricedBag { pbagHeader = BagHeader { .. }, pbagSize, pbagDoses }) = do
    putStrLn ∘ unpack $ "Name = " ⊕ bagName ⊕ " owner = " ⊕ bagOwner ⊕ " cost = $" ⊕ (pack ∘ show ∘ unMoney $ bagCost) ⊕ " size = " ⊕ (pack ∘ show ∘ unGrams $ pbagSize)
    forM_ pbagDoses $ \ (PricedDose { pdose = Dose { .. }, pdoseCost }) →
        putStrLn ∘ unpack $ "    " ⊕ dosePerson ⊕ "\t" ⊕ (pack ∘ show ∘ unGrams $ doseAmount) ⊕ "\t$" ⊕ (pack ∘ show ∘ unMoney $ pdoseCost)
-}

priceBag ∷ Bag → PricedBag
priceBag (Bag { bagHeader = bagHeader@BagHeader { .. }, bagDoses }) =
    PricedBag bagHeader (Grams ∘ changeFixed $ totalGrams) (fmap priceDose bagDoses)
    where
        changeFixed ∷ (Real a, Fractional b) ⇒ a → b
        changeFixed = fromRational ∘ toRational

        totalGrams ∷ Micro
        totalGrams = sum ∘ fmap (changeFixed ∘ unGrams ∘ doseAmount) $ bagDoses

        priceDose dose@(Dose { doseAmount }) = PricedDose dose $ Money (changeFixed doseAmount ÷ totalGrams) ⋅ bagCost

summarizeBag ∷ PricedBag → Map Text (Grams, Money)
summarizeBag (PricedBag { pbagDoses }) =
    Map.unionsWith (⊕) $ fmap doseToMap pbagDoses
    where
        doseToMap (PricedDose { pdose = Dose { .. }, pdoseCost }) = Map.singleton dosePerson (doseAmount, pdoseCost)

parseBag ∷ (Applicative m, Monad m) ⇒ Sink Text m Bag
parseBag = do
    header ← parseBagHeader =≪ maybe (fail "empty input file!") pure =≪ CL.head
    fmap (\ doses → Bag header doses) $ CL.mapMaybeM parseDose =$= CL.consume

parseBagHeader ∷ (Applicative m, Monad m) ⇒ Text → m BagHeader
parseBagHeader line =
    case splitOn "\t" line of
        [name, owner, costText] →
            case readEither (unpack costText) of
                Left err → fail $ "failed to parse bag cost due to " ⊕ err ⊕ ": " ⊕ unpack costText
                Right cost → pure $ BagHeader name owner (Money cost)
        other →
            fail $ "unexpected number of tokens in bag header: " ⊕ show other

parseDose ∷ (Applicative m, Monad m) ⇒ Text → m (Maybe Dose)
parseDose line =
    case splitOn "\t" line of
        []           → pure Nothing
        [strip → ""] → pure Nothing
        [who, amountText] →
            case readEither (unpack amountText) of
                Left err → fail $ "failed to parse entry amount due to " ⊕ err ⊕ ": " ⊕ unpack amountText
                Right amount → pure ∘ Just $ Dose who (Grams amount)
        other →
            fail $ "unexpected number of tokens in log entry: " ⊕ show other
