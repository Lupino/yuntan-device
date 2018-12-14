#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <hs_mqtt.h>


#include "Device/MQTT_stub.h"

pthread_t publish_daemon;

/**
 * @brief A structure that I will use to keep track of some data needed
 *        to setup the connection to the broker.
 *
 * An instance of this struct will be created in my \c main(). Then, whenever
 * \ref reconnect_client is called, this instance will be passed.
 */
struct reconnect_state_t {
    const char* hostname;
    const char* port;
    const char* clientId;
    const char* username;
    const char* password;
    uint8_t* sendbuf;
    size_t sendbufsz;
    uint8_t* recvbuf;
    size_t recvbufsz;
};

void reconnect_client(struct mqtt_client* client, void **reconnect_state_vptr) {
  struct reconnect_state_t *reconnect_state = *((struct reconnect_state_t**) reconnect_state_vptr);

  /* Close the clients socket if this isn't the initial reconnect call */
  if (client->error != MQTT_ERROR_INITIAL_RECONNECT) {
      close(client->socketfd);
  }

  /* Perform error handling here. */
  if (client->error != MQTT_ERROR_INITIAL_RECONNECT) {
      printf("reconnect_client: called while client was in error state \"%s\"\n",
             mqtt_error_str(client->error)
      );
  }

  /* Open a new socket. */
  int sockfd = open_nb_socket(reconnect_state->hostname, reconnect_state->port);
  if (sockfd == -1) {
      perror("Failed to open socket: ");
      exit(EXIT_FAILURE);
  }

  /* Reinitialize the client. */
  mqtt_reinit(client, sockfd,
              reconnect_state->sendbuf, reconnect_state->sendbufsz,
              reconnect_state->recvbuf, reconnect_state->recvbufsz
  );

  /* Send connection request to the broker. */
  mqtt_connect(client, reconnect_state->clientId, NULL, NULL, 0,
          reconnect_state -> username, reconnect_state -> password, 0, 400);

  /* Subscribe to the topic. */

  char topic[100] = {0};

  // Response topic
  // /:key/:uuid/response/:responseid
  sprintf(topic, "/%s/+/response/+", reconnect_state->clientId);
  mqtt_subscribe(client, topic, 0);

  // /:key/:uuid/attributes
  // device report attributes to this topic
  sprintf(topic, "/%s/+/attributes", reconnect_state->clientId);
  mqtt_subscribe(client, topic, 0);

  // /:key/:uuid/telemetry
  // device report telemetry data to this topic
  sprintf(topic, "/%s/+/telemetry", reconnect_state->clientId);
  mqtt_subscribe(client, topic, 0);
}

void publish_callback(void** unused, struct mqtt_response_publish *published) {
  /* note that published->topic_name is NOT null-terminated (here we'll change it to a c-string) */
  char* topic_name = (char*) malloc(published->topic_name_size + 1);
  char* message = (char*) malloc(published->application_message_size + 1);
  memcpy(topic_name, published->topic_name, published->topic_name_size);
  memcpy(message, published->application_message, published->application_message_size);
  topic_name[published->topic_name_size] = '\0';
  message[published->application_message_size] = '\0';
  on_message(topic_name, message);
  free(topic_name);
  free(message);
}

int client_refresher(struct mqtt_client * client) {
  while(1) {
    mqtt_sync(client);
    usleep(100000U);
  }
  return 0;
}

void * publish_worker(void* arg) {
  struct mqtt_client * client = (struct mqtt_client *) arg;
  char *topic;
  char *payload;
  while(1) {
    get_message(&topic, &payload);
    mqtt_publish(client, topic, (void *)payload, strlen(payload) + 1, MQTT_PUBLISH_QOS_0);
  }
  return NULL;
}

int hs_mqtt_main(const char *addr, const char *port,
        const char * clientId, const char * username, const char * password) {

  /* build the reconnect_state structure which will be passed to reconnect */
  struct reconnect_state_t reconnect_state;
  reconnect_state.hostname = addr;
  reconnect_state.port = port;
  reconnect_state.clientId = clientId;
  reconnect_state.username = username;
  reconnect_state.password = password;
  uint8_t sendbuf[2048];
  uint8_t recvbuf[1024];
  reconnect_state.sendbuf = sendbuf;
  reconnect_state.sendbufsz = sizeof(sendbuf);
  reconnect_state.recvbuf = recvbuf;
  reconnect_state.recvbufsz = sizeof(recvbuf);

  /* setup a client */
  struct mqtt_client client;

  mqtt_init_reconnect(&client,
                      reconnect_client, &reconnect_state,
                      publish_callback
  );

  if (pthread_create(&publish_daemon, NULL, publish_worker, &client)) {
    fprintf(stderr, "Failed to start publish_worker.\n");
  }

  /* block */
  client_refresher(&client);
  return 0;
}
