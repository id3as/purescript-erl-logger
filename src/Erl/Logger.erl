-module(logger@foreign).

-export([
         emergency/2,
         alert/2,
         critical/2,
         error/2,
         warning/2,
         notice/2,
         info/2,
         debug/2,
         spyImpl/2,
         unsafeGetCurrentLocation/0,
         unsafeGetCallingLocation/0,
         'emergency\''/3,
         'alert\''/3,
         'critical\''/3,
         'error\''/3,
         'warning\''/3,
         'notice\''/3,
         'info\''/3,
         'debug\''/3,
         addLoggerContext/1,
         getPrimaryLevelImpl/8,
         setPrimaryLevelImpl/1,
         getHandlerLevelImpl/9,
         setHandlerLevelImpl/2
        ]).

-define(do_effectful_log_with_location(Level, Metadata, Report, Loc0),
  fun() ->
    {Module, Fun, Arity, File, Line} = Loc0,
    case logger:allow(Level, Module) of
      true ->
        Loc = #{mfa => {Module, Fun, Arity},
                      line => Line,
                      file => File},

        ErlMetadata = purs_metadata_to_erl(Metadata),

        apply(logger, macro_log, [Loc, Level, Report, ErlMetadata]),
        unit;
      false ->
        unit
    end  
  end).

-define(do_effectful_log(Level, Metadata, Report), 
    [{current_stacktrace, Stack}] = erlang:process_info(self(), [current_stacktrace]),
    fun() ->
      {Module, Fun, Arity, File, Line} = walk_stack(Stack, 0),
      case logger:allow(Level, Module) of
        true ->
          Location = #{mfa => {Module, Fun, Arity},
                        line => Line,
                        file => File},

          ErlMetadata = purs_metadata_to_erl(Metadata),

          apply(logger, macro_log, [Location, Level, Report, ErlMetadata]),
          unit;
        false ->
          unit
      end
    end
  ).

emergency(Metadata, Report) ->
  ?do_effectful_log(emergency, Metadata, Report).

alert(Metadata, Report) ->
  ?do_effectful_log(alert, Metadata, Report).

critical(Metadata, Report) ->
  ?do_effectful_log(critical, Metadata, Report).

error(Metadata, Report) ->
  ?do_effectful_log(error, Metadata, Report).

warning(Metadata, Report) ->
  ?do_effectful_log(warning, Metadata, Report).

notice(Metadata, Report) ->
  ?do_effectful_log(notice, Metadata, Report).

info(Metadata, Report) ->
  ?do_effectful_log(info, Metadata, Report).

debug(Metadata, Report) ->
  ?do_effectful_log(debug, Metadata, Report).

spyImpl(Metadata, Report) ->
  ?do_effectful_log(notice, Metadata, Report).

unsafeGetCurrentLocation() ->
  [{current_stacktrace, Stack}] = erlang:process_info(self(), [current_stacktrace]),
  walk_stack(Stack, 0).

unsafeGetCallingLocation() ->
  [{current_stacktrace, Stack}] = erlang:process_info(self(), [current_stacktrace]),
  walk_stack(Stack, 1).


'emergency\''(Location,Metadata, Report) ->
  ?do_effectful_log_with_location(emergency, Metadata, Report, Location).

'alert\''(Location,Metadata, Report) ->
  ?do_effectful_log_with_location(alert, Metadata, Report, Location).

'critical\''(Location,Metadata, Report) ->
  ?do_effectful_log_with_location(critical, Metadata, Report, Location).

'error\''(Location,Metadata, Report) ->
  ?do_effectful_log_with_location(error, Metadata, Report, Location).

'warning\''(Location,Metadata, Report) ->
  ?do_effectful_log_with_location(warning, Metadata, Report, Location).

'notice\''(Location,Metadata, Report) ->
  ?do_effectful_log_with_location(notice, Metadata, Report, Location).

'info\''(Location,Metadata, Report) ->
  ?do_effectful_log_with_location(info, Metadata, Report, Location).

'debug\''(Location, Metadata, Report) ->
  ?do_effectful_log_with_location(debug, Metadata, Report, Location).


addLoggerContext(LoggerContext) ->
  fun() ->
      ok = logger:update_process_metadata(LoggerContext)
  end.

getPrimaryLevelImpl(Emergency, Alert, Critical, Error, Warning, Notice, Info, Debug) ->
  fun() ->
    level_to_purs(Emergency, Alert, Critical, Error, Warning, Notice, Info, Debug, maps:get(level, logger:get_primary_config()))
  end.

setPrimaryLevelImpl(Level) ->
  fun() ->
    ok = logger:set_primary_config(level, Level)
  end.

getHandlerLevelImpl(Emergency, Alert, Critical, Error, Warning, Notice, Info, Debug, HandlerId) ->
  fun() ->
    {ok, Map} = logger:get_handler_config(HandlerId),
    level_to_purs(Emergency, Alert, Critical, Error, Warning, Notice, Info, Debug, maps:get(level, Map))
  end.

setHandlerLevelImpl(HandlerId, Level) ->
  fun() ->
    ok = logger:set_handler_config(HandlerId, level, Level)
  end.

%%------------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------------
level_to_purs(Emergency, Alert, Critical, Error, Warning, Notice, Info, Debug, Level) ->
  case Level of
    emergency -> Emergency;
    alert -> Alert;
    critical -> Critical;
    error -> Error;
    warning -> Warning;
    notice -> Notice;
    info -> Info;
    debug -> Debug;
    all -> Debug
  end.

purs_metadata_to_erl(Metadata) ->

  #{type := Type} = Metadata,

  Metadata2 = Metadata#{type => case Type of
                                  {trace} -> trace;
                                  {event} -> event;
                                  {command} -> command;
                                  {audit} -> audit
                                end},

  Metadata3 = case maps:get(event, Metadata2, undefined) of
                {Event} -> maps:put(event, Event, Metadata2);
                _ -> Metadata2
              end,

  Metadata4 = case maps:get(text, Metadata3, undefined) of
                undefined -> Metadata3;
                Value -> maps:put(text, binary_to_list(Value), Metadata3)
              end,

  Metadata4.

walk_stack([_LoggerFrame | Stack = [{TopModule, TopFun, TopArity, [{file, TopFile}, {line, TopLine}]} | _]], Skip) ->
  walk_stack_internal({TopModule, TopFun, TopArity, TopFile, TopLine}, Stack, Skip).

walk_stack_internal(Default, [{Module, Fun, Arity, [{file, File}, {line, Line}]} | Rest], Skip) ->
  ModuleStr = atom_to_list(Module),
  case Module =:= logger@ps of
    true -> walk_stack_internal(Default, Rest, Skip);
    false -> 
      ModuleStr = atom_to_list(Module),
      case string:find(ModuleStr, "@ps") of
        "@ps" when Skip =:= 0 ->
          {format(ModuleStr), Fun, Arity, File, Line};
        "@ps" when Skip > 0 ->
          walk_stack_internal(Default, Rest, Skip-1);
        _ ->
          walk_stack_internal(Default, Rest, Skip)
      end
  end;
walk_stack_internal(Default, [], _Skip) -> Default.

format(Str) ->
  list_to_atom(string:join([camel(Token) || Token <- string:tokens(Str, "_")], ".")).

camel([H | T]) when $a =< H, H =< $z ->
  [H - 32 | T];
camel(Other) ->
  Other.
