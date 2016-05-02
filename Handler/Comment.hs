module Handler.Comment where

import Import
import Database.Persist
import Database.Persist.Types

import Database.Persist.Sqlite


getCommentR :: ArticleId -> Handler Html
--getCommentR = defaultLayout [whamlet| get coments|]
getCommentR articleId = do
    article <- runDB $ get404 articleId
    comments <- runDB $ selectList [CommentArticleId ==.  articleId] [Desc CommentText]
    defaultLayout $ do
        $(widgetFile "showcomments")


postCommentR :: Handler Html
postCommentR = defaultLayout [whamlet| post coments|]
