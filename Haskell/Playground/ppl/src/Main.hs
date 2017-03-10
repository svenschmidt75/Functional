module Main where

import Options.Applicative as Opt
import Data.Monoid


data Commands = Add String String
              | Phone String String
              | Show String
              | Email String String
              deriving (Show, Eq)


parserAdd :: Parser Commands
parserAdd = Add <$> strArgument (metavar "FILENAME")
                <*> strArgument (metavar "PERSON_NAME")

parserPhone :: Parser Commands
parserPhone = Phone <$> strArgument (metavar "FILENAME")
                    <*> strArgument (metavar "PHONE_NUMBER")

parserShow :: Parser Commands
parserShow = Show <$> strArgument (metavar "FILENAME")

parserEmail :: Parser Commands
parserEmail = Email <$> strArgument (metavar "FILENAME")
                    <*> strArgument (metavar "EMAIL_ADDRESS")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parserCommand :: Parser Commands
parserCommand = subparser $
        command "add" (parserAdd `withInfo` "Add an entry.")
     <> command "phone" (parserPhone `withInfo` "Add phone number.")
     <> command "show" (parserShow `withInfo` "Show record.")
     <> command "email" (parserEmail `withInfo` "Add email address.")

parserInfoCommand :: ParserInfo Commands
parserInfoCommand = info (parserCommand <**> helper) (progDesc "Manage address book")

main :: IO ()
main = do
    command <- execParser parserInfoCommand
    print command

-- stack exec -- ppl show julie
