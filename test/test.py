import requests
from urllib.parse import urlencode
import json

host = 'http://127.0.0.1:3000'

def api(uri, query = None):
    if query:
        return "{}/api/{}/?{}".format(host, uri, urlencode(query))

    return "{}/api/{}".format(host, uri)

def preprocess(rsp):
    print(rsp.text)
    data = rsp.json()
    if data.get('err'):
        raise Exception(data['err'])
    return data

def check_equal(a, b, key = None):
    if key is None:
        if a != b:
            raise Exception("Value must Equal. except: {} but got {}".format(a, b))
    else:
        if a[key] != b[key]:
            raise Exception("Value must Equal. except: {} but got {}".format(a[key], b[key]))

def run_post(uri, data = None, query=None):
    rsp = requests.post(api(uri, query), data = data)
    return preprocess(rsp)

def run_get(uri, query = None):
    rsp = requests.get(api(uri, query))
    return preprocess(rsp)

def run_delete(uri, query = None):
    rsp = requests.delete(api(uri, query))
    return preprocess(rsp)


def create_device(username, token, type = 'default'):
    return run_post("devices", {'username': username, 'token': token, 'type': type})

def update_token(uuidOrToken, token):
    return run_post("devices/{}/token".format(uuidOrToken), {'token': token})

def update_meta(uuidOrToken, meta):
    return run_post("devices/{}/meta".format(uuidOrToken), {'meta': json.dumps(meta)})

def update_type(uuidOrToken, type):
    return run_post("devices/{}/type".format(uuidOrToken), {'type': type})

def update_username(uuidOrToken, username):
    return run_post("devices/{}/username".format(uuidOrToken), {'username': username})

def get_devices(type = '', from_ = 0, size = 100):
    return run_get("devices", {'type': type, 'from': from_, 'size': size})

def get_devices_by_username(username, type = '', from_ = 0, size = 100):
    return run_get("users/{}/devices".format(username), {'type': type, 'from': from_, 'size': size})

def remove_device(uuidOrToken):
    return run_delete("devices/{}".format(uuidOrToken))

def get_device(uuidOrToken):
    return run_get("devices/{}".format(uuidOrToken))['device']


def clean_devices():
    ret = get_devices()
    for device in ret['devices']:
        ret = remove_device(device['token'])
        check_equal(ret, {'result': 'OK'}, 'result')

def check_devices(total):
    from_ = 0
    size = 100
    ret = get_devices(from_ = from_, size = size)
    check_equal(ret['total'], total)
    check_equal(ret['size'], size)
    check_equal(ret['from'], from_)


def main():
    token1 = "token"
    token2= "token"
    username1 = 'Lupino'
    username2 = 'Lupino'

    clean_devices()

    check_devices(0)
    device1 = create_device(username1, token1)
    check_devices(1)

    device2 = get_device(device1['uuid'])
    check_equal(device1, device2, 'id')
    check_equal(device1, device2, 'token')
    check_equal(device1, device2, 'username')
    check_equal(device1, device2, 'type')
    check_equal(token1, device1['token'])
    check_equal(token1, device2['token'])
    check_equal(username1, device1['username'])
    check_equal(username1, device2['username'])
    check_equal("default", device2['type'])

    # test update meta
    ret = update_meta(token1, {'test1': "test"})
    check_equal(ret, {'result': 'OK'}, 'result')
    device4 = get_device(token1)
    check_equal(device4['meta']['test1'], "test")

    ret = update_meta(token1, {'test1': "test1"})
    check_equal(ret, {'result': 'OK'}, 'result')
    device4 = get_device(token1)
    check_equal(device4['meta']['test1'], "test1")


    ret = update_username(token1, username2)
    check_equal(ret, {'result': 'OK'}, 'result')

    ret = update_type(token1, 'test')
    check_equal(ret, {'result': 'OK'}, 'result')

    ret = update_token(token1, token2)
    check_equal(ret, {'result': 'OK'}, 'result')

    device4 = get_device(token2)
    check_equal(device4['username'], username2)
    check_equal(device4['type'], "test")

    clean_devices()

    check_devices(0)

main()
