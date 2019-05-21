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

import           Data.Int                (Int64)
import           Haxl.Core               (GenHaxl, dataFetch, uncachedRequest)
import           Yuntan.Types.HasMySQL   (HasMySQL)

import           Device.DataSource
import           Device.Types

import           Data.Text               (Text)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

createTable :: HasMySQL u => GenHaxl u Int64
createTable = uncachedRequest CreateTable

createDevice :: HasMySQL u => UserName -> Token -> Type ->  GenHaxl u DeviceID
createDevice un t tp = uncachedRequest (CreateDevice un t tp)

getDevice :: HasMySQL u => DeviceID -> GenHaxl u (Maybe Device)
getDevice devid = dataFetch (GetDevice devid)

getDevIdByToken :: HasMySQL u => Token -> GenHaxl u (Maybe DeviceID)
getDevIdByToken t = dataFetch (GetDevIdByToken t)

getDevIdByUuid :: HasMySQL u => UUID -> GenHaxl u (Maybe DeviceID)
getDevIdByUuid uuid = dataFetch (GetDevIdByUuid uuid)

getDevIdList :: HasMySQL u => From -> Size -> OrderBy -> GenHaxl u [DeviceID]
getDevIdList f s o = dataFetch (GetDevIdList f s o)

countDevice :: HasMySQL u => GenHaxl u Int64
countDevice = dataFetch CountDevice

getDevIdListByName :: HasMySQL u => UserName -> From -> Size -> OrderBy -> GenHaxl u [DeviceID]
getDevIdListByName un f s o = dataFetch (GetDevIdListByName un f s o)

countDeviceByName :: HasMySQL u => UserName -> GenHaxl u Int64
countDeviceByName un = dataFetch (CountDeviceByName un)

getDevIdListByType :: HasMySQL u => Type -> From -> Size -> OrderBy -> GenHaxl u [DeviceID]
getDevIdListByType tp f s o = dataFetch (GetDevIdListByType tp f s o)

countDeviceByType :: HasMySQL u => Type -> GenHaxl u Int64
countDeviceByType tp = dataFetch (CountDeviceByType tp)

getDevIdListByNameAndType :: HasMySQL u => UserName -> Type -> From -> Size -> OrderBy -> GenHaxl u [DeviceID]
getDevIdListByNameAndType un tp f s o = dataFetch (GetDevIdListByNameAndType un tp f s o)

countDeviceByNameAndType :: HasMySQL u => UserName -> Type -> GenHaxl u Int64
countDeviceByNameAndType un tp = dataFetch (CountDeviceByNameAndType un tp)

updateDevice :: HasMySQL u => DeviceID -> String -> Text -> GenHaxl u Int64
updateDevice devid f t = uncachedRequest (UpdateDevice devid f t)

removeDevice :: HasMySQL u => DeviceID -> GenHaxl u Int64
removeDevice devid = uncachedRequest (RemoveDevice devid)
