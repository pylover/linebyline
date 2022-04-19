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
  
  assertEqual (Group (Only "foo" :| [Only "bar"])) $ (Only "foo") <> (Only "bar")

test_tokenizer = do
  assertEqual (tokenize "" " Foo Bar Baz ") ["Foo", "Bar", "Baz"]
  -- assertEqual (tokenize "" "Foo (Bar Baz)") ["Foo", "(", "Bar", "Baz"]


