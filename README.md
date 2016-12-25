# cgate
cgate is a bridge between RabbitMQ and Kafka which forwards messages from specified RMQ exchanges to specific Kafka topics.

## Setup
Configuration for the bridge is described in `rel/sys.config`, under channels subsection. Configure your channels by example:
```
...
{channels, [
  {channel_login, 
   #{from => {rmq, subscribe,
              [#{connection => "amqp://user:password@rabbitmqhost/vhost",
                 exchange => <<"Your.Exchange.To.Read.From">>,
                 routing_key => <<"*">>}]},
     from_decoder => {converter_module_name, deocder_fun_name},
     to_encoder => {converter_module_name, encoder_fun_name},
     to => {kafka, publish,
            [#{kafka_client => kafka_client_1,
               topics => [#{key => <<"routing_key_for_topic">>,
                            partition => 0,
                            topic => <<"topic_to_write_to">>}]}]}
    }
  }
 ]}
...
```

## Tests
In order to run all project tests run:
```
make tests
```

## Usage
After you confgured cgate using `rel/sys.config`, build and start release:
```
make run
```

