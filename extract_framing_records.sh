sed -nr 's#-record\(('.+')(, \{.*)#  ,\1#p' ./rabbit_common/include/rabbit_framing.hrl ./amqp_client/include/amqp_client.hrl
