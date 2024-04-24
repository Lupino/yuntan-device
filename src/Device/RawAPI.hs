module Device.RawAPI
  ( createTable
  , createDevice
  , getDevice
  , getDevIdByToken
  , getDevIdByUuid
  , getDevIdList
  , getDevIdListByKey
  , countDevice
  , countDeviceByKey
  , updateDevice
  , removeDevice
  , getDevKeyId
  , getDevKeyById
  ) where

import           Data.Int            (Int64)
import           Data.Text           (Text)
import           Database.PSQL.Types (From, HasPSQL, OrderBy, Size)
import           Device.DataSource
import           Device.Types
import           Haxl.Core           (GenHaxl, dataFetch, uncachedRequest)

createTable :: HasPSQL u => GenHaxl u w Int64
createTable = uncachedRequest CreateTable

createDevice :: HasPSQL u => KeyID -> Token ->  GenHaxl u w DeviceID
createDevice kid t = uncachedRequest (CreateDevice kid t)

getDevice :: HasPSQL u => DeviceID -> GenHaxl u w (Maybe Device)
getDevice devid = dataFetch (GetDevice devid)

getDevIdByToken :: HasPSQL u => Token -> GenHaxl u w (Maybe DeviceID)
getDevIdByToken t = dataFetch (GetDevIdByToken t)

getDevIdByUuid :: HasPSQL u => UUID -> GenHaxl u w (Maybe DeviceID)
getDevIdByUuid uuid = dataFetch (GetDevIdByUuid uuid)

getDevIdList :: HasPSQL u => From -> Size -> OrderBy -> GenHaxl u w [DeviceID]
getDevIdList f s o = dataFetch (GetDevIdList f s o)

countDevice :: HasPSQL u => GenHaxl u w Int64
countDevice = dataFetch CountDevice

getDevIdListByKey :: HasPSQL u => KeyID -> From -> Size -> OrderBy -> GenHaxl u w [DeviceID]
getDevIdListByKey kid f s o = dataFetch (GetDevIdListByKey kid f s o)

countDeviceByKey :: HasPSQL u => KeyID -> GenHaxl u w Int64
countDeviceByKey kid = dataFetch (CountDeviceByKey kid)

updateDevice :: HasPSQL u => DeviceID -> String -> Text -> GenHaxl u w Int64
updateDevice devid f t = uncachedRequest (UpdateDevice devid f t)

removeDevice :: HasPSQL u => DeviceID -> GenHaxl u w Int64
removeDevice devid = uncachedRequest (RemoveDevice devid)

getDevKeyId :: HasPSQL u => Key -> GenHaxl u w KeyID
getDevKeyId key = dataFetch (GetDevKeyID key)

getDevKeyById :: HasPSQL u => KeyID -> GenHaxl u w Key
getDevKeyById kid = dataFetch (GetDevKeyByID kid)
