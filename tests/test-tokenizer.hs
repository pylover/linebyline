{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Data.List.NonEmpty

import HackLine.Tokenizer

main = htfMain htf_thisModulesTests

test_tokenizer_dump = do
  assertEqual "" $ dump (Empty :: Token String)
  assertEqual "foo" $ dump (Only "foo")
  assertEqual "foo" $ dump (Group (Only "foo" :| []))
  assertEqual "(foo bar)" $ dump (Group $ Only "foo" :| [Only "bar"])
  assertEqual "(foo (bar baz))" $ dump
      (Group ((Only "foo") :| [Group ((Only "bar") :| [Only "baz"])]))

test_tokenizet_monoid = do
  assertEqual Empty $ Empty <> (Empty :: Token String)
  assertEqual (Only "foo") $ Empty <> Only "foo"
  assertEqual (Only "foo") $ Only "foo" <> Empty

  assertEqual (Group (Only "foo" :| [Only "bar", Only "baz"])) 
    $ Only "foo" <> Group (Only "bar" :| [Only "baz"])

  assertEqual (Group (Only "bar" :| [Only "baz", Only "foo"])) 
    $ Group (Only "bar" :| [Only "baz"]) <> Only "foo"

  assertEqual (Group (Only "foo" :| [Only "bar", Only "baz"])) 
    $ Group (Only "foo" :| []) <> Group (Only "bar" :| [Only "baz"])
  
  assertEqual (Group (Only "foo" :| [Only "bar"])) $ (Only "foo") 
    <> (Only "bar")

test_tokenizer = do
  assertEqual ((dump.tokenize "") " Foo Bar Baz ") "(Foo Bar Baz)"

  assertEqual (tokenize "" " Foo Bar Baz ") 
    $ Group ((Only "Foo") :| [Only "Bar", Only "Baz"])

  assertEqual (Group (Only "Foo" :| [Group (Only "Bar" :| [Only "Baz"])])) $
    tokenize "" "Foo (Bar Baz)"
     
  assertEqual (Group (Only "Foo" :|
       [Group (Only "Bar" :| [Group (Only "Baz," :| [Only "qux"])])])) $
    tokenize "" "Foo (Bar (Baz, qux))"

test_mPar = do
  assertEqual ("foo", "") (mPar 0 "foo" "")   
  assertEqual ("foo (bar) baz", "") (mPar 0 "" "foo (bar) baz)")
  assertEqual ("foo (bar) baz", " qux (quux)") 
    (mPar 0 "" "foo (bar) baz) qux (quux)")
