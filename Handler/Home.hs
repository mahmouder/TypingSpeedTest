module Handler.Home where

import Import
import Yesod.Form.Bootstrap3
-- import Text.Julius (RawJS (..))
import System.Random
import Data.List hiding (take, lines, zip, length, insert)


-- Define our data that will be used for creating the form.
-- data TypingTest = TypingTest
--     { sample :: Textarea
--     }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

userForm :: AForm Handler User
userForm = User
        <$> areq textField textFieldSettings Nothing
        <*> areq hiddenField hiddenFieldSettings Nothing
        <*> areq hiddenField hiddenFieldSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textFieldSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Just "username"
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                ]
            }
          hiddenFieldSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control hidden")
                ]
            }

randomList :: Int -> IO [Int]
randomList n = do
    g <- newStdGen
    return . take n . nub $ (randomRs (0,20000) g :: [Int])

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype)<- generateFormPost $ renderBootstrap3 BootstrapBasicForm userForm
    allScores <- runDB $ selectList [] [Desc UserWpm, LimitTo 10]
    file <- liftIO $ try $ readFile "static/20k.txt"
    let wordsList = ["abc", "def", "ghi"] :: [String]
    case file :: Either IOException String of
        Left e -> do
            $logError $ "Could not read data file!!!"
            defaultLayout $ do
                aDomId <- newIdent
                setTitle "Typing Speed Test"
                $(widgetFile "homepage")
        Right str -> do
            let ys = lines str
            xs <- liftIO $ randomList 350
            let wordsList = [y | (i, y) <- zip [0..(length ys -1)] ys, x <- xs, i == x]
            -- $logInfo $ "Successfully read the file!"
            defaultLayout $ do
                aDomId <- newIdent
                setTitle "Typing Speed Test"
                $(widgetFile "homepage")
                -- [whamlet|
                --     <div .masthead>
                --         <div .container>
                --             <div .row>
                --                 <div id="main-textarea" rows="3">
                --                     $forall word <- wordsList
                --                         <span class="word">#{word}

                --     <div .container>
                -- |]
    -- defaultLayout $ do
    --     aDomId <- newIdent
    --     setTitle "Typing Speed Test"
    --     $(widgetFile "homepage")


postHomeR :: Handler Html
postHomeR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm userForm
    case res of
        FormSuccess user -> do
            _ <- runDB $ insert user
            redirect $ HomeR
        _ -> error "Input is missing!!"
