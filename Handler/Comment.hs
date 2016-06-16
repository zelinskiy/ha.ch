module Handler.Comment where

import Import
import Database.Persist
import Database.Persist.Types

import Database.Persist.Sqlite


import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App






entryForm :: ArticleId -> Form Comment
entryForm articleId = renderDivs $ Comment
    <$> pure   articleId
    <*> areq   textField "Text" Nothing
    


getCommentR :: ArticleId -> Handler Html
getCommentR articleId = do
    article <- runDB $ get404 articleId
    (commentWidget, enctype) <- generateFormPost (entryForm articleId)
    comments <- runDB $ selectList [CommentArticleId ==.  articleId] [Desc CommentId]
    defaultLayout $ do
        $(widgetFile "showcomments")






postCommentR :: ArticleId -> Handler Html
postCommentR articleId = do
    ((res,commentWidget),enctype) <- runFormPost (entryForm articleId)
    case res of 
         FormSuccess comment -> do 
            commentId <- runDB $ insert comment
            redirect $ CommentR articleId
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"


