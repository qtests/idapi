{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Control.Lens
import           Data.Swagger
--import           Data.Text(Text(..))
import           Servant                         hiding (Handler)
import           Servant.Swagger

import           AppM
import           Database
import           Handlers

readerServer :: Config -> Server CombinedAPI
readerServer cfg = hoistServer (Proxy :: Proxy CombinedAPI) (makeNat cfg) server

type CompanyAPI = AddCompany
         :<|> UpdateCompany
         :<|> GetCompanies
         :<|> GetCompany
         :<|> DeleteCompany

companyApi :: ServerT CompanyAPI AppM
companyApi = addCompany
    :<|> updateCompany
    :<|> getCompanies
    :<|> getCompanyModel
    :<|> deleteCompany

-- Person
type PersonAPI = AddPerson
            :<|> UpdatePerson
            :<|> GetPerson
            :<|> GetPersons
            :<|> GetPersonCompanies

personApi :: ServerT PersonAPI AppM
personApi = addPerson
       :<|> updatePerson
       :<|> getPerson
       :<|> getPersons
       :<|> getPersonCompanies

-- Story
type StoryAPI = AddStory
         :<|> UpdateStory
         :<|> GetStories
         :<|> GetStory
--         :<|> DeleteStory

storyApi :: ServerT StoryAPI AppM
storyApi = addStory
    :<|> updateStory
    :<|> getStories
    :<|> getStory
--    :<|> deleteStory

-- Combined
type CombinedAPI = CompanyAPI
             :<|> PersonAPI
             :<|> StoryAPI
             :<|> WithHeader
             :<|> ReturnHeader
             :<|> CaseError

server :: ServerT CombinedAPI AppM
server = companyApi 
    :<|> personApi
    :<|> storyApi
    :<|> withHeader
    :<|> responseHeader
    :<|> caseError

-- Swagger Docs
getSwagger :: Swagger
getSwagger = toSwagger (Proxy :: Proxy CombinedAPI)
  & basePath .~ Just "/api"
  & info.title   .~ "Todo API"
  & info.version .~ "1.0"
  & applyTags [Tag "API Controller" (Just "API Controller Name") Nothing]
