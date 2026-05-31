FROM alpine:3.23.4
COPY dist/musl64/yuntan-device /usr/bin/
COPY config.sample.yaml /config.yaml

ENTRYPOINT ["/usr/bin/yuntan-device"]

CMD ["-c", "/config.yaml"]
