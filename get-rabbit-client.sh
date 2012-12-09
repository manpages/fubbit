if [ ! -d "deps" ]; then
  mkdir "deps"
fi

if [ ! -d "amqp_client" ]; then
  cd deps ;\
  hg clone http://hg.rabbitmq.com/rabbitmq-codegen ;\
  hg clone http://hg.rabbitmq.com/rabbitmq-server ;\
  hg clone http://hg.rabbitmq.com/rabbitmq-erlang-client ;\
  cd rabbitmq-erlang-client ;\
  make ;\
  cd .. ;\
  rm -rf amqp_client-* ;\
  rm -rf rabbit_common-* ;\
  
  cp -R rabbitmq-erlang-client/dist/amqp_client-*/ amqp_client ;\
  cp -R rabbitmq-erlang-client/dist/rabbit_common-*/ rabbit_common
fi
