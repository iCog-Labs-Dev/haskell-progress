{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.HTTP.Types

main = scotty 3000 $ do
  get "/" $ do                         
    text "Hi!"     
  -- named parameters:
  get "/askfor/:word" $ do
    w <- param "word"
    html $ mconcat ["<h1>You asked for ", w, ", you got it!</h1>" ]
  -- unnamed parameters from a query string or a form:
  post "/submit" $ do  -- e.g. http://server.com/submit?name=somename
    name <- param "name"
    text name
  -- match a route regardless of the method
  matchAny "/all" $ do
    text "matches all methods"
  -- handler for when there is no matched route
  -- (this should be the last handler because it matches all routes)
  notFound $ do
    text "there is no such route."