
# Build config
Types and building description.
```erlang
ProcName :: atom()
ProdGroupName :: atom()
ProdGroupNames :: [ProdGroupName]
Children :: [Child]
Opts :: map() | [term()]
Child :: {ProcessModule, Opts} | {part_sup, {Name, Children}}
```
Add this to your `sys.config` file: 
```erlang
{smart_house_node,[
{config, Children}
```
## Generic supervisor 
This supervisor is used to group processes logically in supervision tree.
```erlang
{part_sup, {Name, Children}}
```
## Producers
These are workers that provide trigger for further system actions.
### clock_tick
This worker sends `tick` message to all local processes which joined `ProdGroupName`.
```erlang
ProcName :: atom()
SendAfter :: non_neg_integer()
ProdGroupName :: atom()
{clock_tick, [ProcName, SendAfter, ProdGroupName]}
```
## Producer-Consumer
### beam_memory
This worker allows to controll BEAM memory.
```erlang
{beam_memory,
	#{
		name => ProcName,
		init_module => beam_memory,
		init_args => [],
        measurement_module => beam_memory,
        measurement_args => [],
        consumer_groups => ProdGroupNames,
        producer_groups => ProdGroupNames
	}},
```
## Consumers
These are the endpoints for the prcessed data.
### console_writer
This worker logs tick or measurement input which it gets.
```erlang
{console_writer, [ProcName, ProdGroupName]}
```
