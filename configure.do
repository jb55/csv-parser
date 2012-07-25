GHC_OPTS=-Wall
CONF_CMD="cabal configure --ghc-options=$GHC_OPTS"

redo-ifchange csv-parser.cabal
$CONF_CMD || cabal install --only-dependencies && $CONF_CMD
