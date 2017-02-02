module Handler.PostNewScore where

import Import
import Yesod.Form.Bootstrap3


userForm :: AForm Handler User
userForm = User
        <$> areq textField textFieldSettings Nothing
        <*> areq hiddenField hiddenFieldSettings Nothing
        <*> areq hiddenField hiddenFieldSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textFieldSettings = FieldSettings
            { fsLabel = "Your name: "
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
                [ ("class", "form-control")
                ]
            }

getPostNewScoreR :: Handler Html
getPostNewScoreR = do
	(widget, enctype)<- generateFormPost $ renderBootstrap3 BootstrapBasicForm userForm
	defaultLayout $ do
		setTitle "Scores"
		$(widgetFile "score")

postPostNewScoreR :: Handler Html
postPostNewScoreR = do
	((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm userForm
	case res of
		FormSuccess user -> do
			userId <- runDB $ insert user
			redirect $ HomeR
		_ -> defaultLayout $(widgetFile "score")

