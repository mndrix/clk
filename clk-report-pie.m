:- module 'clk-report-pie'.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module assoc_list.
:- import_module exception, float, int, list, map, math, parser, string, term.

:- type summary == map(string,float).

main(!IO) :-
    command_line_arguments(Args,!IO),
    ( [File]=Args -> see(File,_,!IO) ; true ),
    slurp(Lines, !IO),
    filter_map(parse_line, Lines, Entries),
    foldl( group_by, Entries, init, Summary ),
    display_graph(Summary, !IO).

:- pred slurp(list(string)::out, io::di, io::uo) is det.
slurp(Lines, !IO) :-
    slurp_([],Backwards,!IO),
    reverse(Backwards,Lines).
slurp_(Accum,Out,!IO) :-
    read_line_as_string(Result,!IO),
    ( eof      = Result, Out = Accum
    ; error(X) = Result, throw(X)
    ; ok(L)    = Result, slurp_([chomp(L)|Accum],Out,!IO)
    ).

:- pred parse_line(string::in,{string,float}::out) is semidet.
parse_line(Line,{Group,Duration}) :-
    [_,_,Seconds,TagsString,Message] = split_at_char('\t', Line),
    remove_suffix(Seconds,"s",DurationString),
    to_float(DurationString,Duration),
    Tags = (TagsString = "" -> [] ; split_at_char(',',TagsString)),
    Group = (if classify(Tags,Message,G) then G else "other: "++Message),
    \+ Group = "out".

:- pred display_graph(summary::in,io::di,io::uo) is det.
display_graph(Summary, !IO) :-
    foldl_values(max_p, Summary, 0.0, Max),
    foldl_values(sum_p, Summary, 0.0, Sum),
    keys_and_values( to_assoc_list(Summary), Ks, Vs),
    Values = map(float_to_string, Vs),
    Keys = map_corresponding(
        (func(K,V)=C:-format("%s (%.0f%%)",[s(K),f(100.0*V/Sum)],C)),
        Ks, Vs),
    format(
        "open 'https://chart.googleapis.com/chart?chds=0,%.0f&cht=p&chs=700x300&chd=t:%s&chl=%s'",
        [
            f(Max),
            s(join_list(",",Values)),
            s(join_list("|",Keys))
        ],
        Cmd
    ),
    call_system(Cmd, _, !IO).
:- pred max_p(float::in,float::in,float::out) is det.
max_p(X,Y,Z) :- Z = max(X,Y).
:- pred sum_p(float::in,float::in,float::out) is det.
sum_p(X,Y,Z) :- Z = X+Y.

:- pred group_by({string,float}::in,summary::in,summary::out) is det.
group_by({Group,Duration},!Summary) :-
    OldDuration = (if search(!.Summary,Group,D) then D else 0.0),
    set(Group,OldDuration+Duration,!Summary).

:- pred classify(list(string)::in,string::in,string::out) is semidet.
classify([],"out","out").
classify(["ndrix"],"email","email").
classify(["ndrix","research"],_,"twenty").
classify(["ndrix","20"],_,"twenty").
classify(["ndrix","coincard"],_,"twenty").
classify(["jjgames"|_],_,"jjgames").
classify(["scs"|_],_,"scs").
classify(["vgpc"|_],_,"vgpc").
