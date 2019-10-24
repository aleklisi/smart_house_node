
# Build config
Types and building description.
```erlang
ProcName :: atom()
ProcGroupName :: atom()
ProcGroupNames :: [ProcGroupName]
Children :: [Child]
Opts :: map() | [term()]
Child :: {ProcessModule, Opts} | {pipe_sup, {Name, Children}}
```
Add this to your `sys.config` file:
```erlang
{smart_house_node,[
{config, Children}]
```
## Generic supervisor
This supervisor is used to group processes logically in supervision tree.
```erlang
{pipe_sup, {Name, Children}}
```
## Producers
These are workers that provide trigger for further system actions.
### clock_tick
This worker sends `tick` message to all local processes which joined `ProcGroupName`.
```erlang
ProcName :: atom()
SendAfter :: non_neg_integer()
ProcGroupName :: atom()
{clock_tick, [ProcName, SendAfter, ProcGroupName]}
```
## Producer-Consumer
### beam_memory
This worker allows to control BEAM memory.
```erlang
{beam_memory,
	#{
		name => ProcName,
		init_module => beam_memory,
		init_args => [],
        measurement_module => beam_memory,
        measurement_args => [],
        consumer_groups => ProcGroupNames,
        producer_groups => ProcGroupNames
	}},
```
### bmp_180
This worker allows to monitor temperature plugged into i2c. Implementation based on the [docs](https://cdn-shop.adafruit.com/datasheets/BST-BMP180-DS000-09.pdf).

WARNING! Currently only temperature is supported only!

```erlang
Device :: "i2c-1" | "i2c-0"

{bmp_180,
	#{
		name => ProcName,
		init_module => bmp_180,
		init_args => [#{device_name => Device}],
        measurement_module => bmp_180,
        measurement_args => [],
        consumer_groups => ProcGroupNames,
        producer_groups => ProcGroupNames
    }},
```

Use `i2cdetect -y 0` or `i2cdetect -y 1` to check which device.
See this for diagnosis https://pinout.xyz/pinout/i2c.

| BMP_180 pin | RPi GPIO | RPi pin |
|---|---|---|
| VIN | 3.3v power | 1 |
| GND | Ground | 6 |
| SCL | BCM 2 (Data) | 5 |
| SDA | BCM 3 (Clock) | 3 |

### cpu_temperature
This worker allows to monitor temperature RPi of CPU.
```erlang
{cpu_temperature,
	#{
		name => ProcName,
		init_module => cpu_temperature,
		init_args => [],
        measurement_module => cpu_temperature,
        measurement_args => [],
        consumer_groups => ProcGroupNames,
        producer_groups => ProcGroupNames
	}},
```

### gpio_state
This worker allows to monitor GPIO input state.
GpioPin is pin number on board see [this](https://www.raspberrypi.org/documentation/usage/gpio/)
When the circuit os opened, returns 1 else returns 0.
```erlang 
GpioPin :: 2 | 3 | 4 ... 27
{gpio_state,
    #{
        name => ProcName,
        init_module => gpio_state,
        init_args => [GpioPin],
        measurement_module => gpio_state,
        measurement_args => [],
        consumer_groups => ProcGroupNames,
        producer_groups => ProcGroupNames
    }},
```


## Consumers
These are the endpoints for the processed data.
### logger_reporter
This worker logs any input it gets.
```erlang
{logger_reporter,
                    #{
                        name => ProcName,
                        reporter_module => logger_reporter,
                        reporter_init_args => [],
                        consumer_group => ProcGroupName
                    }
                }
```

### exometer_reporter
This worker sends data through exometer to graphite.
Requires exomiter application up and running.
`MaybeDiagramType` is an empty list or a 1 element list containing exometer probe type, see [this](https://github.com/Feuerlabs/exometer#built-in-entries-and-probes) for further details. `histogram` is a default.
```erlang
MaybeDiagramType = [] | [ counter | fast_counter | gauge | histogram ]
{exometer_reporter,
                    #{
                        name => ProcName,
                        reporter_module => exometer_reporter,
                        reporter_init_args => [],
                        consumer_group => ProcGroupName
                    }
                }
```
