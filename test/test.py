import requests
from urllib.parse import urlencode
import json
from uuid import uuid4
from time import time

host = 'http://127.0.0.1:3000'

def api(uri, query = None):
    if query:
        return "{}/api/{}?{}".format(host, uri, urlencode(query))

    return "{}/api/{}".format(host, uri)

def preprocess(rsp):
    if rsp.status_code != 200:
        print(rsp)
        print(rsp.url)

    print(rsp.content)
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


def create_device(key, token):
    return run_post("devices", {'key': key, 'token': token})

def update_attr(ident, attr, value):
    return run_post(f"devices/{ident}/{attr}/", {attr: value})

def update_token(ident, token):
    return update_attr(ident, 'token', token)

def update_meta(ident, meta):
    return run_post("devices/{}/meta/".format(ident), {'meta': json.dumps(meta)})

def get_devices(from_ = 0, size = 100, key='', index_name='', idents=[], gw_id=0):
    return run_get("devices/", {
        'from': from_,
        'size': size,
        'key': key,
        'index_name': index_name,
        'idents': ','.join(idents),
        'gw_id': gw_id
    })

def remove_device(ident):
    return run_delete("devices/{}/".format(ident))

def get_device(ident):
    return run_get("devices/{}/".format(ident))['device']

def save_card(ident, param, meta = {}):
    return run_post(f"devices/{ident}/cards/", {'meta': json.dumps(meta), 'param': param})

def remove_card(ident, param):
    return run_delete(f"devices/{ident}/cards/{param}/")

def save_metric(ident, metric):
    return run_post(f"devices/{ident}/metric/", {'metric': json.dumps(metric)})

def remove_metric(ident, param, mid):
    return run_delete(f"devices/{ident}/metric/{param}/{mid}/")

def drop_metric(ident, param):
    return run_delete(f"devices/{ident}/metric/{param}/")

def get_metric_list(ident, param, from_ = 0, size = 10, started_at = 0, ended_at = 0, sort='asc'):
    return run_get(f"devices/{ident}/metric/{param}/", {
        'from': from_,
        'size': size,
        'started_at': started_at,
        'ended_at': ended_at,
        'sort': sort,
    })


def save_index(ident, index_name):
    return run_post(f"devices/{ident}/index/", {'index_name': index_name})

def remove_index(ident, index_name):
    return run_post(f"devices/{ident}/index/delete/", {'index_name': index_name})

def drop_index(ident):
    return run_post(f"devices/{ident}/index/drop/")

def drop_all_index(index_name):
    return run_post(f"index/drop/", {'index_name': index_name})

def clean_devices():
    ret = get_devices()
    for device in ret['devices']:
        ret = remove_device(device['uuid'])
        check_equal(ret, {'result': 'OK'}, 'result')

def check_devices(total):
    from_ = 0
    size = 100
    ret = get_devices(from_ = from_, size = size)
    check_equal(ret['total'], total)
    check_equal(ret['size'], size)
    check_equal(ret['from'], from_)


def update_attr_and_check(ident, attr, value):
    ret = update_attr(ident, attr, value)
    check_equal(ret, {'result': 'OK'}, 'result')

    device = get_device(ident)
    check_equal(device[attr], value)


def test_cards(ident):
    param = 'temperature'
    meta = {'test': 'just a test'}
    ret = save_card(ident, param, meta)
    check_equal(ret['param'], param)
    check_equal(ret['meta'], meta, "test")

    device = get_device(ident)
    check_equal(len(device['cards']), 1)
    card = device['cards'][0]

    check_equal(card['param'], param)
    check_equal(card['meta'], meta, "test")

    ret = remove_card(ident, param)
    check_equal(ret, {'result': 'OK'}, 'result')

    device = get_device(ident)
    check_equal(len(device['cards']), 0)


def test_metrics(ident):
    metrics = []
    total = 10
    for i in range(total):
        metrics.append({'temperature': 28.2 + i, 'index': i, 'created_at': i})

    save_metric(ident, metrics)


    device = get_device(ident)
    check_equal(device['metric']['index'], 9)
    check_equal(device['metric']['temperature'], 37.2)

    ret = get_metric_list(ident, 'temperature')
    check_equal(ret['total'], total)

    check_equal(ret['data'][0]['created_at'], 0)
    check_equal(ret['data'][-1]['created_at'], 9)

    ret = get_metric_list(ident, 'temperature', sort='desc')
    check_equal(ret['total'], total)
    data = ret['data']

    check_equal(data[0]['created_at'], 9)
    check_equal(data[-1]['created_at'], 0)

    ret = remove_metric(ident, 'temperature', data[0]['id'])
    check_equal(ret, {'result': 'OK'}, 'result')
    ret = remove_metric(ident, 'temperature', data[-1]['id'])
    check_equal(ret, {'result': 'OK'}, 'result')

    device = get_device(ident)
    check_equal(device['metric']['index'], 9)
    check_equal(device['metric']['temperature'], 36.2)

    ret = get_metric_list(ident, 'index', sort='desc')
    check_equal(ret['total'], total)
    data = ret['data']

    check_equal(data[0]['value'], 9)
    check_equal(data[-1]['value'], 0)

    ret = drop_metric(ident, 'index')
    check_equal(ret, {'result': 'OK'}, 'result')

    ret = get_metric_list(ident, 'index', sort='desc')
    check_equal(ret['total'], 0)

    device = get_device(ident)
    check_equal(device['metric'].get('index'), None)
    check_equal(device['metric']['temperature'], 36.2)

def test_index(ident):
    index_name0 = 'index_0'
    index_name1 = 'index_1'
    index_name2 = 'index_2'
    ret = save_index(ident, index_name0)
    check_equal(ret, {'result': 'OK'}, 'result')
    ret = save_index(ident, index_name1)
    check_equal(ret, {'result': 'OK'}, 'result')
    ret = save_index(ident, index_name2)
    check_equal(ret, {'result': 'OK'}, 'result')

    device = get_device(ident)
    indexs = device['index']
    check_equal(len(indexs), 3)
    check_equal(indexs[0]['name'], index_name2)
    check_equal(indexs[1]['name'], index_name1)
    check_equal(indexs[2]['name'], index_name0)

    ret = remove_index(ident, index_name2)
    check_equal(ret, {'result': 'OK'}, 'result')

    device = get_device(ident)
    indexs = device['index']
    check_equal(len(indexs), 2)
    check_equal(indexs[0]['name'], index_name1)
    check_equal(indexs[1]['name'], index_name0)

    ret = drop_index(ident)
    check_equal(ret, {'result': 'OK'}, 'result')

    device = get_device(ident)
    indexs = device['index']
    check_equal(len(indexs), 0)

    key = '1234567890abcdef'
    idents = []
    for i in range(10):
        token = f'test_index_token_{i}'
        device = create_device(key, token)
        indexs = device['index']
        check_equal(len(indexs), 0)
        uuid = device['uuid']
        idents.append(uuid)

        ret = save_index(uuid, index_name2)
        check_equal(ret, {'result': 'OK'}, 'result')

        device = get_device(uuid)
        indexs = device['index']
        check_equal(len(indexs), 1)

    ret = get_devices(idents=idents)
    check_equal(ret['total'], 10)

    ret = get_devices(index_name=index_name2)
    check_equal(ret['total'], 10)

    ret = drop_all_index(index_name2)
    check_equal(ret, {'result': 'OK'}, 'result')

    try:
        get_devices(index_name=index_name2)
    except Exception as exc:
        check_equal(str(exc), 'index_name is invalid')


def main():
    token1 = "test_token1"
    token2 = "test_token2"


    key1 ='1234567890abcdef'
    key2 ='1234567890abcggg'

    username1 = 'Lupino'
    username2 = 'Lupino'

    clean_devices()

    check_devices(0)
    device1 = create_device(key1, token1)
    check_devices(1)

    device2 = get_device(device1['uuid'])
    check_equal(device1, device2, 'id')
    check_equal(device1, device2, 'token')
    check_equal(device1, device2, 'key')
    check_equal(device1, device2, 'addr')
    check_equal(token1, device1['token'])
    check_equal(token1, device2['token'])
    check_equal(key1, device1['key'])
    check_equal(key1, device2['key'])

    # test update meta
    ret = update_meta('token_' + token1, {'test1': "test你"})
    check_equal(ret, {'result': 'OK'}, 'result')
    device4 = get_device('token_' + token1)
    check_equal(device4['meta']['test1'], "test你")

    ret = update_meta('token_' + token1, {'test1': "test1"})
    check_equal(ret, {'result': 'OK'}, 'result')
    device4 = get_device('token_' + token1)
    check_equal(device4['meta']['test1'], "test1")

    uuid = str(uuid4())
    update_attr_and_check('token_' + token1, 'uuid', uuid)

    update_attr_and_check(uuid, 'addr', '123456')
    update_attr_and_check(uuid, 'gw_id', 12)
    update_attr_and_check(uuid, 'token', token2)
    update_attr_and_check(uuid, 'ping_at', int(time()))

    test_cards(uuid)
    test_metrics(uuid)

    test_index(uuid)

    clean_devices()

    check_devices(0)

main()
