FROM erlang:23-alpine

RUN mkdir workroot
WORKDIR workroot

COPY . .

RUN rebar3 compile

RUN rebar3 eunit --dir="test"

CMD [ "rebar3", "shell" ]