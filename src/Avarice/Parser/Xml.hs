{-# LANGUAGE InstanceSigs #-}
module Avarice.Parser.Xml (xmlParseFile, parseXmlSource, fxml, simpleName) where --Avarice.System.Parser

import Control.Monad ()
import System.IO ( IOMode(ReadMode), hClose, hGetContents, openBinaryFile )  
import qualified Text.XML.Light.Input  as XML
import qualified Text.XML.Light.Lexer  as XML
import qualified Text.XML.Light.Proc   as XML
import qualified Text.XML.Light.Types  as XML

-- raw XML
{-
  <hgscript>
  <text width="96">血粉が…</text><newline />
  <text width="336">まるでオーラのように見える…</text><newline />
  <text width="264">喜んで…　いるのか…？</text>
  </hgscript>
-}

fxml :: [String] -> [String]
fxml = map read

simpleName :: String -> XML.QName 
simpleName s  = XML.QName s Nothing Nothing 

xmlParseFile :: IO ()
xmlParseFile = do  
        handle   <- openBinaryFile "C:/Users/Manda/OneDrive/Desktop/avarice/examples/basic/hgtest.xml" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents 
            list        = fxml singlewords
        print list
        hClose handle   

parseXmlSource :: XML.XmlSource s => s -> IO ()
parseXmlSource source = do
    let contents = XML.parseXML source
        quotes   = concatMap (XML.findElements $ simpleName "hgscript") (XML.onlyElems contents)
        symbols  = map       (XML.findAttr $ simpleName "width") quotes
    print symbols