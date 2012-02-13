module SP.Syntax where

import SP.ByteString

data Article = Article {artId::Int, artSnts::[Sentence]}
data Sentence = Sentence {sntId::Int, tkns::[Token], deps::[Dependency]}
data Token = Token {tknId::Int, pos, lemma, ner::ByteString}
data Dependency = Dependency {rel::ByteString, gov, dpt::Token}
