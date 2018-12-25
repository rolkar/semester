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
    NextBank = maps:put(acc, erlang:round(math:floor(NextAcc)), Bank),
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

command({convert}, State = #{acc := RawAcc, bank := Bank}) ->
    #{acc := Acc, normal := Normal} = Bank,
    NewBank = Bank#{acc => 0, normal => Normal + Acc},
    NewRawAcc = RawAcc - Acc,
    State#{acc => NewRawAcc, bank => NewBank};
command({off, Days}, State = #{bank := Bank}) ->
    #{pre := Pre, normal := Normal, used_pre := UPre} = Bank,
    case Pre of
	0 ->
	    State#{bank => Bank#{normal => Normal - Days}};
	Pre when Days =< Pre ->
	    State#{bank => Bank#{pre => Pre - Days,
				 used_pre => UPre + Days}};
	Pre ->
	    State#{bank => Bank#{pre => 0,
				 normal => Days - Pre,
				 used_pre => UPre + Pre}}
    end;
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
    io:format("~10s ~10s ~4s ~4s ~4s ~4s ~4s ~4s~n",
	      ["", "", "Acc", "Norm", "Pre", "UPre", "Pacc", "Psav"]).

display_bank(Date, Bank, PrevBank) ->
    case Bank == PrevBank of
	true ->
	    ignore;
	false ->
	    Acc = maps:get(acc, Bank),
	    Normal = maps:get(normal, Bank),
	    Pre = maps:get(pre, Bank),
	    UPre = maps:get(used_pre, Bank),
	    Pacc = maps:get(ps_acc, Bank),
	    Psav = maps:get(ps_sav, Bank),

	    io:format("~s ~10s ~4w ~4w ~4w ~4w ~4w ~4w~n",
		      [pretty_date(Date), "", Acc, Normal, Pre, UPre,
		       Pacc, Psav])
    end.

pretty_date({Year, Month, Day}) ->
    io_lib:format("~p-~2..0w-~2..0w", [Year, Month, Day]).
