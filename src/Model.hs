{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Model where

import           Data.Swagger
import           Database
import           Database.Persist.Sql
import           GHC.Generics
import           GHC.Int
import           Yesod
import           Data.Text(Text(..))

class ApiModel a e | a -> e where
  toEntity :: a -> e
  toUpdate :: a -> [Update e]
  fromEntity :: Entity e -> a


-- Person
data PersonModel = PersonModel { id      :: Maybe Int64
                               , name    :: String
                               , address :: Maybe String
                               , companyId   :: Maybe Int64
                               } deriving Generic

instance ToSchema PersonModel
instance ToJSON PersonModel
instance FromJSON PersonModel
instance ApiModel PersonModel Person where
  toEntity (PersonModel _ name' address' companyId') = Person name' address' (toSqlKey <$> companyId')
  toUpdate (PersonModel _ name' address' companyId') = [ PersonName =. name'
                                                   , PersonAddress =. address'
                                                   , PersonCompanyId =. toSqlKey <$> companyId'
                                                   ]
  fromEntity e = let value = entityVal e
                 in  PersonModel { id = Just $ fromSqlKey (entityKey e)
                                 , name = personName value
                                 , address = personAddress value
                                 , companyId = fromSqlKey <$> personCompanyId value
                                 }

-- Company
data CompanyModel = CompanyModel 
                    { 
                        id :: Maybe Int64, 
                        name :: Text,
                        url :: Text,
                        ticker :: Text,
                        status :: Bool
                    } deriving (Generic)

instance ToSchema CompanyModel
instance ToJSON CompanyModel
instance FromJSON CompanyModel
instance ApiModel CompanyModel Company where
    toEntity (CompanyModel _ name' url' ticker' status') = Company name' url' ticker' status'
    toUpdate (CompanyModel _ name' url' ticker' status') = [ CompanyName     =. name'
                                                            , CompanyUrl     =. url'
                                                            , CompanyTicker  =. ticker'
                                                            , CompanyStatus  =. status'
                                                            ]
    fromEntity e = let value = entityVal e
                    in CompanyModel { id = Just $ fromSqlKey (entityKey e)
                                 , name = companyName value
                                 , url = companyUrl value
                                 , ticker = companyTicker value
                                 , status = companyStatus value
                                 }

