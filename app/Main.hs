{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Yesod

newtype Live = Live ConnectionPool

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Post
  author Text
  content Text
  deriving Show
|]

mkYesod
  "Live"
  [parseRoutes|
/ HomeR GET POST
/post/#PostId PostR GET
|]

instance Yesod Live where
  defaultLayout widget = do
    let style :: Widget = toWidget [lucius| h2 { color: green; } |]
    pc <- widgetToPageContent $ widget <> style
    withUrlRenderer
      [hamlet|
                $doctype 5
                <html>
                    <head>
                        <title>#{pageTitle pc}
                        ^{pageHead pc}
                        <meta name=keywords content="some sample keywords">
                    <body>
                        ^{pageBody pc}
            |]

instance YesodPersist Live where
  type YesodPersistBackend Live = SqlBackend

  runDB action = do
    Live pool <- getYesod
    runSqlPool action pool

instance RenderMessage Live FormMessage where
  renderMessage _ _ = defaultFormMessage

getPostR :: PostId -> Handler Html
getPostR postId = do
  post <- runDB $ selectList [PostId ==. postId] [LimitTo 1]
  case post of
    [] -> notFound
    p : _ ->
      defaultLayout $ do
        setTitle . toHtml . ("Post by " <>) . postAuthor . entityVal $ p
        drawPost (entityVal p) Nothing
        homeHyper
        postStyle

getHomeR :: Handler Html
getHomeR = do
  (widget, enctype) <- generateFormPost postForm
  posts <- runDB $ selectList [] []
  defaultLayout $ do
    setTitle "Live"
    submitForm widget enctype
    drawPosts posts
    postStyle

postHomeR :: Handler Html
postHomeR = do
  ((result, _), _) <- runFormPost postForm
  case result of
    FormSuccess post -> do
      key <- runDB $ insert post
      liftIO $ print key
      return ()
    _ -> return ()
  redirect HomeR

homeHyper :: Widget
homeHyper = toWidget [hamlet|<a href=@{HomeR}>go back|]

postStyle :: Widget
postStyle =
  toWidget
    [lucius|
  .post { 
    background: green;
    margin: 5px 0px 5px 0px;
  }
  .post-author { color: #aaa }
  .post-id { color: #aaa }
|]

postForm :: Html -> MForm Handler (FormResult Post, Widget)
postForm =
  renderDivs $
    Post
      <$> areq textField "Author" Nothing
      <*> areq textField "Text" Nothing

drawPost :: Post -> Maybe PostId -> Widget
drawPost (Post author content) postId = do
  toWidget
    [hamlet|
    $maybe i <- postId
      <a href=@{PostR i}>
        <div .post >
          <p .post-fill> #{content}
          <p .post-author> by <b>#{author}</b>
    $nothing
      <div .post >
        <p .post-fill> #{content}
        <p .post-author> by <b>#{author}</b>
    |]

drawPosts :: [Entity Post] -> Widget
drawPosts = foldr (flip (>>) . wrap drawPost) header
  where
    header = toWidget [hamlet|<h2> Posts:|]
    wrap f e = f (entityVal e) $ Just (entityKey e)

submitForm :: Widget -> Enctype -> Widget
submitForm form enc = do
  toWidget
    [whamlet|
  <h2> Submit: 
  <form method=post action=@{HomeR} enctype=#{enc}>
            ^{form}
            <button>Submit
  |]

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = do
  runStderrLoggingT $ withSqlitePool "state/db.sqlite" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
      runMigration migrateAll
    warp 3000 $ Live pool
