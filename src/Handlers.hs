{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handlers where

import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Text(Text(..))
import qualified Database.Esqueleto         as E
import           Database.Persist.Sqlite
import           GHC.Int                    (Int64)
import           Servant                    hiding (Handler)

import           AppM
import           Database
import           Model


type GetPersonCompanies = "person" :> "companies" :> Get '[JSON] [(Int64, Maybe Text)]

getPersonCompanies :: AppM [(Int64, Maybe Text)]
getPersonCompanies = runDb $ do
  rows <- E.select $
               E.from $ \(person `E.LeftOuterJoin` company)  -> do
                 E.on (person E.^. PersonCompanyId E.==. E.just (company E.^. CompanyId))
                 return (person E.^. PersonId, E.just (company E.^. CompanyName))

  let tuples = map (\(a,b) -> ( (fromSqlKey . E.unValue) a, E.unValue b)  ) rows

  return tuples



instance MimeRender PlainText Int64 where
  mimeRender _ v = pack $ show v

type AddCompany =   "company" :> "add"
             :> ReqBody '[JSON] CompanyModel
             :> Post '[PlainText] Int64

addCompany :: CompanyModel -> AppM Int64
addCompany cpModel = do
  entity <- runDb $ insert $ toEntity cpModel
  return $ fromSqlKey entity

type UpdateCompany =   "company" :> "update"
             :> Capture "id" Int64
             :> ReqBody '[JSON] CompanyModel
             :> Post '[PlainText] NoContent

updateCompany :: Int64 -> CompanyModel -> AppM NoContent
updateCompany i cpModel = do
  runDb $ update (toSqlKey i) (toUpdate cpModel)
  return NoContent


type DeleteCompany = "company" :> "delete"
              :> Capture "id" Int64
              :> Delete '[PlainText] NoContent

deleteCompany :: Int64 -> AppM NoContent
deleteCompany i = do
  runDb $ delete (toSqlKey i :: Key Company)
  return NoContent


type GetCompanies = "company"  :> "list" :> Get '[JSON] [CompanyModel]
getCompanies :: AppM [CompanyModel]
getCompanies = runDb $ do
    b <- selectList [] [Desc CompanyId]
    return $ map fromEntity b

type GetCompany = "company" :> "get" :> Capture "id" Int64 :> Get '[JSON] CompanyModel
getCompanyModel :: Int64 -> AppM CompanyModel
getCompanyModel i = do
  entity <- runDb $ selectFirst [CompanyId <-. [toSqlKey i]] []
  case entity of
        (Just x) -> return $ fromEntity x
        Nothing  -> throwError err404  { errBody = "Company not found" }

-- Person
type AddPerson =  "person" :> "add"
                :> ReqBody '[JSON] PersonModel
                :> Post '[PlainText] String

addPerson :: PersonModel -> AppM String
addPerson model = do
  key <- runDb $ insert $ toEntity  model
  return $ show (fromSqlKey key)

type UpdatePerson =    "person" :> "update"
                    :> Capture "id" Int64
                    :> ReqBody '[JSON] PersonModel
                    :> Post '[PlainText] NoContent

updatePerson :: Int64 -> PersonModel -> AppM NoContent
updatePerson i model = do
  runDb $ update (toSqlKey i) (toUpdate model)
  return NoContent


type GetPerson =  "person" :> "get"
               :> Capture "id" Int64
               :> Get '[JSON] PersonModel

getPerson :: Int64 -> AppM PersonModel
getPerson i = do
  entity <- runDb $ selectFirst [PersonId <-. [toSqlKey i :: Key Person]] []
  case entity of
        (Just x) -> return $ fromEntity x
        Nothing  -> throwError err404  { errBody = "Person not found" }


type GetPersons = "person"  :> "list" :> Get '[JSON] [PersonModel]
getPersons :: AppM [PersonModel]
getPersons = runDb $ do
  list <- selectList [] [Asc PersonId]
  return $ map fromEntity list

-- Story
type AddStory =  "story" :> "add"
                :> ReqBody '[JSON] StoryModel
                :> Post '[PlainText] String

addStory :: StoryModel -> AppM String
addStory model = do
  key <- runDb $ insert $ toEntity  model
  return $ show (fromSqlKey key)

type UpdateStory =    "story" :> "update"
                    :> Capture "id" Int64
                    :> ReqBody '[JSON] StoryModel
                    :> Post '[PlainText] NoContent

updateStory :: Int64 -> StoryModel -> AppM NoContent
updateStory i model = do
  runDb $ update (toSqlKey i) (toUpdate model)
  return NoContent


type GetStory =  "story" :> "get"
               :> Capture "id" Int64
               :> Get '[JSON] StoryModel

getStory :: Int64 -> AppM StoryModel
getStory i = do
  entity <- runDb $ selectFirst [StoryId <-. [toSqlKey i :: Key Story]] []
  case entity of
        (Just x) -> return $ fromEntity x
        Nothing  -> throwError err404  { errBody = "Story not found" }


type GetStories = "story"  :> "list" :> Get '[JSON] [StoryModel]

getStories :: AppM [StoryModel]
getStories = runDb $ do
  list <- selectList [] [Asc StoryId]
  return $ map fromEntity list

-- End Story


type CaseError = "get1" :> Capture "str" String :> Get '[PlainText] String
caseError :: String -> AppM String
caseError str = case str of
                  "404" -> throwError err404
                  "401" -> throwError err401
                  "500" -> throwError err500
                  _     -> return str


-- Request handlers
type WithHeader = "with-header"       :> Servant.Header "Header" String
                                      :> Get '[PlainText] String
withHeader :: Maybe String -> AppM String
withHeader = return . show
---
type ReturnHeader =     "return-header"
                     :> Get '[PlainText] (Headers '[Servant.Header "SomeHeader" String] String)

responseHeader :: AppM (Headers '[Servant.Header "SomeHeader" String] String)
responseHeader = return $ Servant.addHeader "headerVal" "foo"
