
FROM erlang:20.2.2

# Updating apt repository of the related OS
RUN apt-get clean && \
    apt-get update && \
    apt-get install -y lsof telnet

ADD . /sample
WORKDIR /sample
RUN rebar3 release