-module(semester).

-export([go/0]).

go() ->
    {ok, Content} = file:read_file("semester.txt"),
    Lines = string:tokens(binary_to_list(Content), "\r\n"),
    Commands = [ extract(L) || L <- Lines ],
    History = history(Commands),
    display_header(),
    display(History, no_bank).

extract(RawLine) ->
    case string:strip(strip_comment(RawLine)) of
	"" ->
	    nop;
	Line ->
	    [Date, Command | _] = string:tokens(Line, " "),
	    #{date => extract_date(Date), command => extract_command(Command)}
    end.

strip_comment(RawLine) ->
    case string:str(RawLine, "#") of
	0 ->
	    RawLine;
	N ->
	    {Line,_} = lists:split(N-1, RawLine),
	    Line
    end.

extract_date(RawDate) ->
    [Year, Month, Day] = string:tokens(RawDate, "-"),
    {list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)}.

extract_command(RawCommand) ->
    case string:tokens(RawCommand, "=") of
	[Command] ->
	    {list_to_atom(Command)};
	[Command, Value] ->
	    {list_to_atom(Command), list_to_integer(Value)}
    end.

history([]) ->
    [];
history(Commands) ->
    First = maps:get(date, hd(Commands)),
    history(Commands,
	    #{date => First,
	      weekdays => 0,
	      work_day_sallary => 0,
	      work_days_per_year => 250,
	      sallary_last_year => 0,
	      bank =>
		  #{days => 0,
		    saved => 0,
		    lost => 0,
		    pre_days => 0,
		    pre_used => 0,
		    pre_saved => 0,
		    ps_days => 0,
		    ps_saved => 0}
	     },
	    []).

history([], _State, Ops) ->
    lists:reverse(Ops);
history([nop|Commands], State, Ops) ->
    history(Commands, State, Ops);
history(Commands = [#{date := CDate, command := Command} | Tail],
	State = #{date := Date},
	Ops) ->
    case Date < CDate of
	true ->
	    NextState = advance_one_day(State), 
	    history(Commands, NextState, Ops);
	false ->
	    NextState = command(Command, State),
	    Bank = maps:get(bank, NextState),
	    history(Tail, NextState, [{CDate,Command,Bank}|Ops])
    end.

advance_one_day(State) ->
    NextDate = next_date(maps:get(date, State)),
    State#{date => NextDate}.
    
next_date(Date) ->
    NextDate = calendar:date_to_gregorian_days(Date)+1,
    calendar:gregorian_days_to_date(NextDate).

command({add, Days}, State) ->
    add_bank(days, Days, State);
command({pre_add, Days}, State) ->
    add_bank(pre_days, Days, State);
command({sallary, Sallary}, State) ->
    State#{work_day_sallary => Sallary};
command({ps, Days}, State) ->
    put_bank(ps_days, Days, State);
command({ps_save, Days}, State) ->
    put_bank(ps_saved, Days, State);
command({new_year}, State) ->
    save_days(compute_work_day_salary(State));
command({weekdays, Days}, State) ->
    convert_weekdays(Days, State);
command({off, Days}, State) ->
    off(Days, State);
command({days, Days}, State) ->
    State#{days => Days};
command(nop, State) ->
    State.

convert_weekdays(NewWeekDays, State = #{weekdays := 0}) ->
    State#{weekdays => NewWeekDays};
convert_weekdays(NewWeekDays, State = #{weekdays := OldWeekDays, bank := Bank}) ->
    %% This is a bit hacky way of converting all days in the bank
    %% according to "nettometoden". Not sure it is actually done this
    %% way, but it is an approximation of the truth, I think.
    #{days := Days, saved := Saved} = Bank,
    Mult = NewWeekDays/OldWeekDays,
    NewBank = Bank#{days => round(Mult*Days), saved => round(Mult*Saved)},
    State#{weekdays => NewWeekDays, bank => NewBank}.

off(0, State) ->
    State;
off(Days, State = #{bank := Bank}) ->
    NewBank = off_one(Bank),
    off(Days-1, State#{bank => NewBank}).

compute_work_day_salary(State) ->
    State.   %% TODO ---- needs to be implemented

save_days(State = #{bank := Bank0}) ->
    #{days := Days,
      saved := Saved,
      lost := Lost,
      pre_days := PreDays,
      pre_saved := PreSaved} = Bank0,
    Bank = Bank0#{pre_days => 0, pre_saved => PreSaved + PreDays},
    case take_five(Days) of
	{0, 0} ->
	    State#{bank => Bank};
	{_, 0} ->
	    State#{bank => Bank#{days := 0, saved := Saved + Days}};
	{D, P} ->
	    State#{bank => Bank#{days := 0, saved := Saved + D, lost := Lost + P}}
    end.

take_five(N) when N < 6 ->
    {N,0};
take_five(N) ->
    {5,N-5}.

off_one(Bank = #{days := 0, saved := 0, pre_days := 0, pre_saved := PreSaved, pre_used := PreUsed}) ->
    Bank#{pre_saved => PreSaved - 1, pre_used => PreUsed + 1};
off_one(Bank = #{days := 0, saved := 0, pre_days := Pre, pre_used := PreUsed}) ->
    Bank#{pre_days => Pre - 1, pre_used => PreUsed + 1};
off_one(Bank = #{days := 0, saved := Saved}) ->
    Bank#{saved => Saved - 1};
off_one(Bank = #{days := Days}) ->
    Bank#{days => Days - 1}.

display([], _) ->
    ok;
display([{Date, advance, Bank} | Ops], PrevBank) ->
    display_bank(Date, Bank, PrevBank),
    display(Ops, Bank);
display([{Date, Op, Bank} | Ops], PrevBank) ->
    display_op(Date, Op),
    display_bank(Date, Bank, PrevBank),
    display(Ops, Bank).

display_op(Date, {Op}) ->
    io:format("~s ~p~n", [pretty_date(Date), Op]);
display_op(Date, {Op,Arg}) ->
    io:format("~s ~p=~p~n", [pretty_date(Date), Op, Arg]).

display_header() ->
    io:format("~10s ~10s ~6s ~6s ~6s - ~6s ~6s ~6s - ~6s ~6s~n",
	      ["", "",
	       "Days", "Saved", "Lost",
	       "PrD", "PrS", "PrUsd",
	       "PsD", "PsS"]).

display_bank(Date, Bank, PrevBank) ->
    case Bank == PrevBank of
	true ->
	    ignore;
	false ->
	    Days = maps:get(days, Bank),
	    Saved = maps:get(saved, Bank),
	    Lost = maps:get(lost, Bank),
	    PreD = maps:get(pre_days, Bank),
	    PreS = maps:get(pre_saved, Bank),
	    PreU = maps:get(pre_used, Bank),
	    PsD = maps:get(ps_days, Bank),
	    PsS = maps:get(ps_saved, Bank),

	    io:format("~s ~10s ~6w ~6w ~6w - ~6w ~6w ~6w - ~6w ~6w~n",
		      [pretty_date(Date), "",
		       Days, Saved, Lost,
		       PreD, PreS, PreU,
		       PsD, PsS])
    end.

pretty_date({Year, Month, Day}) ->
    io_lib:format("~p-~2..0w-~2..0w", [Year, Month, Day]).

put_bank(Key, Value, State = #{bank := Bank}) ->
    State#{bank => maps:put(Key, Value, Bank)}.

add_bank(Key, Value, State = #{bank := Bank}) ->
    State#{bank => add_map(Key, Value, Bank)}.

add_map(Key, Value, Map) ->
    maps:put(Key, maps:get(Key, Map, 0) + Value, Map).
