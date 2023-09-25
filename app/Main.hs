module Main (main) where

import Control.Exception (bracket)
import Control.Monad (msum)
import Control.Monad.Reader (asks)
import Control.Monad.State (get, put)
import Data.Acid
  ( AcidState,
    Query,
    Update,
    makeAcidic,
    openLocalState,
  )
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.Data (Data)
import Data.Dynamic (Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text)
import Happstack.Server (BodyPolicy, Browsing (DisableBrowsing), Method (GET, POST), decodeBody, defaultBodyPolicy, dir, lookText', method, nullConf, ok, seeOther, serveDirectory, simpleHTTP)
import Happstack.Server.SimpleHTTP
  ( Response,
    ServerPartT,
    ToMessage (toResponse),
  )
import Text.Blaze.Html5 (Html, toHtml, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

-- STATE

data Post = Post
  { author :: Text,
    content :: Text
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Post)

newtype PostState = PostState {ps :: [Post]}
  deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''PostState)

initialPostState :: PostState
initialPostState = PostState posts
  where
    posts :: [Post]
    posts = [Post "admin" "Hello, World!", Post "admin" "More data!"]

newPost :: Post -> Update PostState [Post]
newPost p = do
  s <- get
  let new = p : ps s
  put $ s {ps = new}
  return new

purge :: Update PostState [Post]
purge = do
  s <- get
  put $ s {ps = []}
  return []

peekPosts :: Query PostState [Post]
peekPosts = asks ps

$(makeAcidic ''PostState ['peekPosts, 'newPost, 'purge])

-- HANDLERS

main :: IO ()
main = withState (simpleHTTP nullConf . homePage)
  where
    withState =
      bracket
        (openLocalState initialPostState)
        createCheckpointAndClose

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

homePage :: AcidState PostState -> ServerPartT IO Response
homePage acid = do
  msum
    [ dir "static" $ serveDirectory DisableBrowsing [] "res",
      -- control
      dir "purge" $ do
        method GET
        _ <- update' acid Purge
        seeOther ("/" :: Text) plug,
      do
        method POST
        decodeBody myPolicy
        getPost >>= seeOther ("/" :: Text),
      -- pages
      method GET >> home
    ]
  where
    home = do
      ps <- query' acid PeekPosts
      ok $ template "live" $ do
        submitForm
        drawPosts ps

    getPost = do
      a <- lookText' "author"
      c <- lookText' "content"
      let p = Post a c
      if valid p
        then update' acid (NewPost p) >> return plug
        else return plug

    valid (Post "" _) = False
    valid (Post _ "") = False
    valid _ = True

    plug = toResponse ()

-- VIEW

drawPost :: Post -> Html
drawPost p = H.div ! A.class_ "post" $ do
  H.p $ toHtml $ content p
  H.p $ toHtml ("by: " :: Text) <> H.b (toHtml $ author p)

drawPosts :: (Foldable a) => a Post -> Html
drawPosts = foldr (flip (>>) . drawPost) header
  where
    header = H.html $ H.h1 "Posts:"

submitForm :: Html
submitForm = do
  H.h1 "Submit:"
  H.form
    ! A.enctype "multipart/formdata"
    ! A.action "/"
    ! A.method "POST"
    $ do
      H.label ! A.for "name" $ "Name:"
      H.input ! A.type_ "text" ! A.name "author" ! A.id "author"
      H.br
      H.label ! A.for "text" $ "Text:"
      H.input ! A.type_ "text" ! A.name "content" ! A.id "content"
      H.br
      H.input ! A.type_ "submit" ! A.value "Submit"

template :: Text -> Html -> Response
template title body = toResponse $
  H.docTypeHtml $ do
    H.head $ do
      H.link ! A.rel "stylesheet" ! A.href "static/styles.css"
      H.link ! A.rel "stylesheet" ! A.href "https://fonts.googleapis.com/css?family=Ubuntu"
      H.title $ toHtml title
    H.body $ do
      body
      footer
  where
    footer = H.div ! A.class_ "footer" $ H.p "don't screw this up"
