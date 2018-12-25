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
	      days => 0,
	      acc => 0.0,
	      bank =>
		  #{acc => 0,
		    normal => 0,
		    pre => 0,
		    used_pre => 0,
		    saved => 0,
		    lost => 0,
		    ps_acc => 0,
		    ps_sav => 0}
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
	    Bank = maps:get(bank, NextState),
	    history(Commands, NextState, [{Date,advance,Bank}|Ops]);
	false ->
	    NextState = command(Command, State),
	    Bank = maps:get(bank, NextState),
	    history(Tail, NextState, [{CDate,Command,Bank}|Ops])
    end.

advance_one_day(State) ->
    NextDate = next_date(maps:get(date, State)),
    AccPerDay = acc_per_day(maps:get(days, State), element(1, NextDate)),
    NextAcc = maps:get(acc, State) + AccPerDay,
    Bank = maps:get(bank, State),
    NextBank = maps:put(acc, erlang:round(NextAcc), Bank),
    State#{date => NextDate, acc => NextAcc, bank => NextBank}.
    
next_date(Date) ->
    NextDate = calendar:date_to_gregorian_days(Date)+1,
    calendar:gregorian_days_to_date(NextDate).

acc_per_day(Days, Year) ->
    Days / days(Year).

days(Year) ->
    First = calendar:date_to_gregorian_days({Year, 1, 1}),
    Last = calendar:date_to_gregorian_days({Year, 12, 31}),
    Last - First + 1.

command({convert}, State) ->
    add_acc(save_days(State));
command({off, Days}, State) ->
    off(Days, State);
command(nop, State) ->
    State;
command({pre, Days}, State = #{bank := Bank}) ->
    #{pre := Pre} = Bank,
    State#{bank => Bank#{pre => Pre + Days}};
command({days, Days}, State) ->
    State#{days => Days};
command({ps_acc, Days}, State = #{bank := Bank}) ->
    NewBank = Bank#{ps_acc => Days},
    State#{bank => NewBank};
command({ps_sav, Days}, State = #{bank := Bank}) ->
    NewBank = Bank#{ps_sav => Days},
    State#{bank => NewBank}.

off(0, State) ->
    State;
off(Days, State = #{bank := Bank}) ->
    NewBank = off_one(Bank),
    off(Days-1, State#{bank => NewBank}).

save_days(State = #{bank := Bank}) ->
    #{normal := Normal, saved := Saved, used_pre := UPre} = Bank,
    case take_five(Normal) of
	{0, 0} ->
	    State;
	{_, 0} ->
	    NewBank = Bank#{normal := 0, saved := Saved + Normal},
	    State#{bank => NewBank};
	{N, P} ->
	    NewBank = Bank#{normal := 0, saved := Saved + N, used_pre := UPre - P},
	    State#{bank => NewBank}
    end.

take_five(N) when N < 6 ->
    {N,0};
take_five(N) ->
    {5,N-5}.

add_acc(State = #{acc := RawAcc, bank := Bank}) ->
    #{acc := Acc, normal := Normal} = Bank,
    NewBank = Bank#{acc => 0, normal => Normal + Acc},
    NewRawAcc = RawAcc - Acc,
    State#{acc => NewRawAcc, bank => NewBank}.

off_one(Bank = #{saved := 0, normal := 0, pre := Pre, used_pre := UPre}) ->
    Bank#{pre => Pre - 1, used_pre => UPre + 1};
off_one(Bank = #{saved := 0, normal := Normal}) ->
    Bank#{normal => Normal -1};
off_one(Bank = #{saved := Saved}) ->
    Bank#{saved => Saved - 1}.

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
    io:format("~10s ~10s ~4s ~4s ~4s - ~4s ~4s ~4s - ~4s ~4s~n",
	      ["", "",
	       "Acc", "Norm", "Pre",
	       "Sav", "Lost", "UPre",
	       "Pacc", "Psav"]).

display_bank(Date, Bank, PrevBank) ->
    case Bank == PrevBank of
	true ->
	    ignore;
	false ->
	    Acc = maps:get(acc, Bank),
	    Normal = maps:get(normal, Bank),
	    Pre = maps:get(pre, Bank),
	    Saved = maps:get(saved, Bank),
	    Lost = maps:get(lost, Bank),
	    UPre = maps:get(used_pre, Bank),
	    Pacc = maps:get(ps_acc, Bank),
	    Psav = maps:get(ps_sav, Bank),

	    io:format("~s ~10s ~4w ~4w ~4w - ~4w ~4w ~4w - ~4w ~4w~n",
		      [pretty_date(Date), "",
		       Acc, Normal, Pre,
		       Saved, Lost, UPre,
		       Pacc, Psav])
    end.

pretty_date({Year, Month, Day}) ->
    io_lib:format("~p-~2..0w-~2..0w", [Year, Month, Day]).
