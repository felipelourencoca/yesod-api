{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls#-}

-- new pragmas for Persistent: enerating Persistent entities now requires the following language extensions:             
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
             
import Yesod
import Database.Persist.MySQL
import Data.Text
import Data.Time.Clock
import Data.Time (Day)
import Control.Monad.Logger (runStdoutLoggingT)
import GHC.Generics
import GHC.Int(Int64)
import           Network.HTTP.Types       (status200, status400)

data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Cliente json
   nome Text 
   UniqueCliente nome
   deriving Show
Conta
   clienteId ClienteId
--   montante Rational -- !
   limiteCredito Double
   montante Double
   cadastroData Day
   deriving Show
   deriving Generic
Operacao json
    contaId ContaId
    descricao Text
    montante Double
    dataCadastro Day
    deriving Show
    
|]

instance FromJSON Conta where
    parseJSON (Object o) = Conta
        <$> o .: "clienteId"
        <*> o .: "limiteCredito"
        <*> o .: "montante"
        <*> o .: "cadastroData"
    parseJSON _ = undefined
    
instance ToJSON Conta
{-
instance FromJSON Operacao where
    parseJSON (Object o) = Operacao
        <$> o .: "contaId"
        <*> o .: "descriacao"
        <*> o .: "dataCadastro"

instance ToJSON Operacao 
-}

mkYesod "Pagina" [parseRoutes|
/ HomeR GET
/cliente                                    ClienteCadastrarR   POST 
!/cliente/#ClienteId                        ClienteR            GET

/cliente/criar-conta                        CriarContaR         POST
!/cliente/mostrar-conta/#ClienteId          MostrarContaR       GET

/cliente/conta/operacao/comprar         ClienteComprarR     POST
/cliente/conta/operacao/sacar           ClienteSacarR       POST
/cliente/conta/operacao/depositar       ClienteDepositarR   POST


|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
      
getHomeR :: Handler ()
getHomeR = sendResponse ( object [ pack "resp" .= object [pack "saudacao" .= pack "Ola Bem vindo ao Nubank"] ] )

-- Cliente ==
getClienteR :: ClienteId -> Handler ()
getClienteR cid = do
    cliente <- (runDB.get404) cid
    sendResponse $ object [pack "cliente" .= toJSON cliente ]


postClienteCadastrarR :: Handler ()
postClienteCadastrarR = do
    cliente <- requireInsecureJsonBody :: Handler Cliente
    cliResult <- runDB $ insertBy cliente
    case cliResult of
        Left (Entity cid cliente) -> sendResponse $ object [ pack "cliente" .= pack "Usuário existente"]
        Right cid -> sendResponse (object [pack "resp" .= pack "Usuário Criado" ] )


-- === CONTA
getMostrarContaR :: ClienteId -> Handler ()
getMostrarContaR cid = do
    mConta <- runDB $ selectFirst [ContaClienteId ==. cid ] [] -- Cliente tendo mais de uma conta, o ideal é usar o selectList e depois filtar a busca.
    case mConta of
        Nothing -> sendResponseStatus status400 (object [pack "" .= pack ""])
        Just (Entity cid conta) -> sendResponse $ object [pack "conta" .= toJSON conta]
    
postCriarContaR :: Handler ()
postCriarContaR = do
    contaCli <- requireJsonBody :: Handler Conta
    liftIO $ putStrLn (show contaCli)
    -- uid <- requireJsonKey "clienteId" contaCli
    cid <- runDB $ insert contaCli
    contaClienteBd <- runDB $ get404 cid
    sendResponse (object [pack "resp" .= toJSON contaClienteBd])

-- Operacoes
postClienteComprarR :: Handler ()
postClienteComprarR = do
    compraJB <- requireJsonBody :: Handler Operacao
    let compraAux = Operacao (operacaoContaId compraJB) (operacaoDescricao compraJB) ( (-1) * operacaoMontante compraJB ) (operacaoDataCadastro compraJB)
    cid <- runDB $ insert compraAux
    compra <- runDB $ get404 cid
    sendResponse $ object [ pack "compra" .= toJSON compra]
    
postClienteSacarR :: Handler ()
postClienteSacarR = do
    retiradaJB <- requireJsonBody :: Handler Operacao
    let retiradaAux = Operacao (operacaoContaId retiradaJB) (operacaoDescricao retiradaJB) ( (-1) * operacaoMontante retiradaJB ) (operacaoDataCadastro retiradaJB)
    rid <- runDB $ insert retiradaAux 
    retirada <- runDB $ get404 rid
    sendResponse $ object [ pack "retirada" .= toJSON retirada]
    
postClienteDepositarR :: Handler ()
postClienteDepositarR = do
    depositoJB <- requireJsonBody :: Handler Operacao
    did <- runDB $ insert depositoJB
    deposito <- runDB $ get404 did
    sendResponse $ object [ "deposito" .= toJSON deposito ]
 
mySqlInfo :: ConnectInfo
mySqlInfo = defaultConnectInfo
  { connectHost     = "127.0.0.1"
  , connectPort     = 3306
  , connectUser     = "etec"
  , connectPassword = "123"
  , connectDatabase = "balances"
  }

main :: IO ()
main = runStdoutLoggingT $
  withMySQLPool mySqlInfo 10 $ \pool -> liftIO $ do
    runSqlPersistMPool (runMigration migrateAll) pool
    warp 3000 (Pagina pool)
 
 {- a função withMySQLPool recebe como parametr um tipo ConectInfo. 
 j-a a withPostgresqlPool recebe uma string.
 
 
connStr = "dbname=dfghhlmdtl4a8s host=ec2-54-243-239-199.compute-1.amazonaws.com user=fnvxvhjtgforpu password=4b2a3523db6d67cced9771a7a17cc696190a4bc84abeaf6177ede7eff298980a port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool  connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)

-}