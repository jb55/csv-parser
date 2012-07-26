
# Data.CSV.Parser

Easily parse CSV files

## Example

```haskell
import Data.CSV.Parser
import Control.Applicative

type Parser a = FromCSV String a

data Track = Track { trackCatId :: String
                   , trackNum   :: Int
                   , trackTitle :: String
                   } deriving (Show)

trackParser :: Parser Track
trackParser = do
  first <- columnInd 0
  catId <- column "CatalogId"
  track <- read <$> column "Track"
  title <- column "Title"
  return $ Track catId track title

main = do
  Right csv <- parseCSVFromFile "stuff.csv"
  mapM_ print (convert trackParser csv)
```
