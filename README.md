# cgate
cgate is a bridge between RabbitMQ and Kafka which forwards messages from specific RMQ exchange to specific Kafka topic.


  - [Setup](#setup)
    - [Kafka](#kafka)
    - [RabbitMQ](#rabbitmq)
    - [Channels](#channels)
  - [Tests](#tests)
  - [Usage](#usage)
    - [Running from the shell](#running-from-the-shell)
    - [Running as a daemon](#running-as-a-daemon)

## Setup
Configuration for the bridge is described in the `rel/sys.config` and in the `tests/test.spec`.There are several subsections for RMQ, Kafka and channels configuration.

### Kafka
```erlang
{brod, [
  {clients, [
    {kafka_client_1, [
      %% hostname or ip as a string, port as a number
      {endpoints, [{"kafka_host", kafka_port}]}, 
      %% ???
      {reconnect_cool_down_seconds, 10},         
      %% ???
      {auto_start_producers, true},              
      %% ???
      {default_producer_config, []}             
    ]}
  ]}
]}
```
### RabbitMQ
This sectioin appears only in test.spec for RMQ connection testing.
```erlang
{rmq_config, 
  {"amqp://user:password@rabbitmqhost/vhost", 
  <<"Exchange.To.Read.From">>}
}
```
### Channels
Channel is an actual bridge between RMQ exchange and Kafka topic. Application may run multiple channels to transfer data from RMQ to Kafka.
```erlang
{channels, [
  {channel_name, 
   #{from => {rmq, subscribe,
              [#{connection => "amqp://user:password@rabbitmqhost/vhost",
                 exchange => <<"Exchange.To.Read.From">>,
                 routing_key => <<"*">>}]},
     %% fun which converts raw data from rmq to erlang term            
     from_decoder => {converter_module_name, decoder_fun_name},   
     %% fun which converts erlang term to kafka string payload   
     to_encoder => {converter_module_name, encoder_fun_name},        
     to => {kafka, publish, 
             %% ???
            [#{kafka_client => kafka_client_1,                       
               topics => [#{key => <<"routing_key_for_the_topic">>,
                            %% ???
                            partition => 0,                          
                            topic => <<"topic_to_write_to">>}]}]}
    }
  }
 ]}
```

## Tests
In order to run all tests run:
```
make tests
```

## Usage
### Running from the shell
After configuring channels in `rel/sys.config`, build and start release:
```
make run
```

### Running as a daemon
1. Setup configuration in `rel/sys.config`
2. Make release `make rel`
3. Unzip release from `_rel/cgate_release/cgate_release-1.tar.gz` to `/opt/cgate`
4. Use service file from `daemon/cgate` to run application as a daemon



