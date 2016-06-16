module Handler.Home

where

import Import
--import Data.Monoid


-- to use Html into forms
import Yesod.Form.Nic (YesodNic, nicHtmlField)
--import Yesod.Message (RenderMessage)

instance YesodNic App





entryForm :: Form Article
entryForm = renderDivs $ Article
    <$> areq textField (FieldSettings "Title" Nothing (Just "NewOpTitleInput") Nothing []) Nothing

    <*> areq textareaField (FieldSettings "Text" Nothing (Just "NewOpTextInput") Nothing [("cols", "30")]) Nothing


-- The view showing the list of articles
getHomeR :: Handler RepHtml
getHomeR = do
    -- Get the list of articles inside the database.
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet).
    (articleWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "articles")


postHomeR :: Handler RepHtml
postHomeR = do
    ((res,articleWidget),enctype) <- runFormPost entryForm
    case res of 
         FormSuccess article -> do 
            articleId <- runDB $ insert article
            setMessage $ toHtml $ (articleTitle article) <> " created"
            redirect $ ArticleR articleId
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "articleAddError")



myForm :: ArticleId -> Form Comment
myForm articleId = renderDivs $ Comment
    <$> pure   articleId
    <*> areq   textField "Text" Nothing



getArticleR :: ArticleId -> Handler RepHtml
getArticleR articleId = do
    article <- runDB $ get404 articleId
    --
    (commentWidget, enctype) <- generateFormPost (myForm articleId)
    comments <- runDB $ selectList [CommentArticleId ==.  articleId] [Desc CommentArticleId]
    --
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")



postArticleR :: ArticleId -> Handler Html
postArticleR articleId = do
    ((res,commentWidget),enctype) <- runFormPost (myForm articleId)
    case res of 
         FormSuccess comment -> do 
            commentId <- runDB $ insert comment
            redirect $ ArticleR articleId
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"        


