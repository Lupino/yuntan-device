module Device.RawAPI
  ( createTable
  , createDevice
  , getDevice
  , getDevIdByToken
  , getDevIdByUuid
  , getDevIdList
  , getDevIdListByName
  , getDevIdListByType
  , getDevIdListByNameAndType
  , countDevice
  , countDeviceByName
  , countDeviceByType
  , countDeviceByNameAndType
  , updateDevice
  , removeDevice
  ) where

import           Data.Int            (Int64)
import           Data.Text           (Text)
import           Database.PSQL.Types (From, HasPSQL, OrderBy, Size)
import           Device.DataSource
import           Device.Types
import           Haxl.Core           (GenHaxl, dataFetch, uncachedRequest)

createTable :: HasPSQL u => GenHaxl u w Int64
createTable = uncachedRequest CreateTable

createDevice :: HasPSQL u => UserName -> Token -> Type ->  GenHaxl u w DeviceID
createDevice un t tp = uncachedRequest (CreateDevice un t tp)

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

getDevIdListByName :: HasPSQL u => UserName -> From -> Size -> OrderBy -> GenHaxl u w [DeviceID]
getDevIdListByName un f s o = dataFetch (GetDevIdListByName un f s o)

countDeviceByName :: HasPSQL u => UserName -> GenHaxl u w Int64
countDeviceByName un = dataFetch (CountDeviceByName un)

getDevIdListByType :: HasPSQL u => Type -> From -> Size -> OrderBy -> GenHaxl u w [DeviceID]
getDevIdListByType tp f s o = dataFetch (GetDevIdListByType tp f s o)

countDeviceByType :: HasPSQL u => Type -> GenHaxl u w Int64
countDeviceByType tp = dataFetch (CountDeviceByType tp)

getDevIdListByNameAndType :: HasPSQL u => UserName -> Type -> From -> Size -> OrderBy -> GenHaxl u w [DeviceID]
getDevIdListByNameAndType un tp f s o = dataFetch (GetDevIdListByNameAndType un tp f s o)

countDeviceByNameAndType :: HasPSQL u => UserName -> Type -> GenHaxl u w Int64
countDeviceByNameAndType un tp = dataFetch (CountDeviceByNameAndType un tp)

updateDevice :: HasPSQL u => DeviceID -> String -> Text -> GenHaxl u w Int64
updateDevice devid f t = uncachedRequest (UpdateDevice devid f t)

removeDevice :: HasPSQL u => DeviceID -> GenHaxl u w Int64
removeDevice devid = uncachedRequest (RemoveDevice devid)
