module Monomer.Parser.Xml where --Avarice.System.Parser
import Data.Text
import Text.Xml

testParse :: XmlSource s => s -> IO ()
testParse = do
    let contents = parseXML source
        quotes   = concatMap (findElements $ simpleName "StockQuote") (onlyElems contents)
        symbols  = map (findAttr $ simpleName "Symbol") quotes
        simpleName s = QName s Nothing Nothing
    print symbols