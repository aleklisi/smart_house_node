# Behaviors

This doc describes behaviors, which are shortcuts to let you write much less code and handle most of the stuff by the framework.

## sensor_behaviour
It is a generic behaviour for handling sensors.
You need to implement following functions:
 - child_spec/1
 - init/1
 - take_measurements/1

### child_spec/1
Returns [childspec](http://erlang.org/doc/man/supervisor.html#type-child_spec) for your worker.

### init/1
Takes a one element list with whatever is passed inside and returns anything.
The init result is stored in `sensor process state` under the key `init_result`.

### take_measurements/1
Takes `state` of `gen_server` and returns a list of tuples where firs element is atom representing measurement name, and second one is value. Eg `{my_count, 12}`.

## reporter_behaviour
It is a generic behaviour for handling data from sensors.
You need to implement following functions:
 - child_spec/1
 - init/1
 - handle_info/2

### child_spec/1
Returns [childspec](http://erlang.org/doc/man/supervisor.html#type-child_spec) for your worker.

### init/1
Takes a one element list with whatever is passed inside and returns anything.
The init result is stored in `sensor process state` under the key `init_result`.

### handle_info/2
Takes given Info and State and should act and than return NewState. If you do not need to effect State just return old one, it does not require to be affected.
