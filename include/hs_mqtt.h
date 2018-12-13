#ifndef __HS_MQTT_H__
#define __HS_MQTT_H__

#include "mqtt.h"
#include "posix_sockets.h"
int hs_mqtt_main(const char *addr, const char *port,
        const char *clientId, const char *username, const char *password);
#endif
