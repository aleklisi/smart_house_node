# Build config

```erlang
{smart_house_node,[
        {config, [
            % {Module, {ProcName, Children}}
		]}
```

## Producers

### Clock producer

```erlang
% {Module, {ProcName, Opts}}
{clocks_sup, 
	{clocks_sup, [
	% {ProcName, [ProcName, SentAfter, Msg, Group]}
		{clock_tick, [tick, 1000, default]},
		{clock_tick, [tock, 500, other]}
	]}}
]}
```

## Consumers

### Console logger

```erlang
% {Module, [ Name, MsgId, GroupName]}
{console_writer, [console_writer, msg_id, group_name]}
```
