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
         addLoggerContext/1,
         getPrimaryLevelImpl/8,
         setPrimaryLevelImpl/1,
         getHandlerLevelImpl/9,
         setHandlerLevelImpl/2
        ]).

-define(do_effectful_log(Level, Metadata, Report),
        [{current_stacktrace, Stack}] = erlang:process_info(self(), [current_stacktrace]),
        fun() ->
            {Module, Fun, Arity, File, Line} = walk_stack(Stack),

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
        end).

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

walk_stack([_LoggerFrame | Stack = [{TopModule, TopFun, TopArity, [{file, TopFile}, {line, TopLine}]} | _]]) ->
  walk_stack_internal({TopModule, TopFun, TopArity, TopFile, TopLine}, Stack).

walk_stack_internal(Default, [{Module, Fun, Arity, [{file, File}, {line, Line}]} | Rest]) ->
  ModuleStr = atom_to_list(Module),
  case string:prefix(ModuleStr, "logger@ps") of
    nomatch ->
      case string:find(ModuleStr, "@ps") of
        "@ps" ->
          {format(ModuleStr), Fun, Arity, File, Line};
        _ ->
          walk_stack_internal(Default, Rest)
      end;
    _ ->
      walk_stack_internal(Default, Rest)
  end;

walk_stack_internal(Default, []) -> Default.

format(Str) ->
  list_to_atom(string:join([camel(Token) || Token <- string:tokens(Str, "_")], ".")).

camel([H | T]) when $a =< H, H =< $z ->
  [H - 32 | T];
camel(Other) ->
  Other.
