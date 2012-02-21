:- module 'clk-report-pie'.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception, float, int, list, map, math, parser, string, term.

:- type summary == map(string,float).

main(!IO) :-
    handle_line( map.init, _Summary, !IO ).

:- pred handle_line( summary::in, summary::out, io::di, io::uo ) is cc_multi.
handle_line(!Summary,!IO) :-
    read_line_as_string(Result,!IO),
    ( eof = Result, display_graph(!.Summary, !IO)
    ; error(X) = Result, throw(X)
    ;
        ok(L) = Result,
        read_cols(L,Seconds,TagsString,Message),
        read_duration(Seconds,Duration),
        read_tags(TagsString,Tags),
        classify(Tags,Message,Group),
        (
        if Group="out"
        then true
        else
%           format("%.1f %s\n", [f(Duration),s(Group)], !IO),
            update_group(Group,Duration,!Summary)
        ),
        handle_line(!Summary,!IO)
    ).

:- pred display_graph(summary::in,io::di,io::uo) is det.
display_graph(Summary, !IO) :-
    Sum = foldl((func(_,V,A) = X :- X=V+A), Summary, 0.0),
    Summary1 = map_values_only((func(V) = V/Sum),Summary),
    foldl2(accum_kv,Summary1,[],Keys,[],Values),
    format(
        "open 'https://chart.googleapis.com/chart?cht=p&chs=500x300&chd=t:%s&chl=%s'",
        [
            s(join_list(",",Values)),
            s(join_list("|",Keys))
        ],
        Cmd
    ),
    call_system(Cmd, _, !IO).

:- pred accum_kv(string,float,list(string),list(string),list(string),list(string)).
:- mode accum_kv(in,in,in,out,in,out) is det.
accum_kv(K,V,A0,[KS|A0],B0,[VS|B0]) :-
    format("%.1f", [f(V)], VS),
    format("%s (%.1f%%)", [s(K),f(100.0*V)], KS).

:- pred read_cols(string::in,string::out,string::out,string::out) is det.
read_cols(L,Seconds,Tags,Message) :-
    if [_,_,S,T,M] = split_at_char('\t', chomp(L))
    then Seconds=S,    Tags=T,  Message=M
    else Seconds="0s", Tags="", Message=""
    .

:- pred read_duration(string::in,float::out) is det.
read_duration(Seconds,Duration) :-
    if remove_suffix(Seconds,"s",DurationString),
       to_float(DurationString,D)
    then Duration = D
    else Duration = 0.0
    .

:- pred read_tags(string::in,list(string)::out) is cc_multi.
read_tags("",[]).
read_tags(TagsString,Tags) :-
    Tags = split_at_char(',',TagsString).

:- pred update_group(string::in,float::in,summary::in,summary::out) is det.
update_group(Group,Duration,!Summary) :-
    OldDuration = (if search(!.Summary,Group,D) then D else 0.0),
    set(Group,OldDuration+Duration,!Summary).

:- pred classify(list(string)::in,string::in,string::out) is cc_multi.
classify([],"out","out").
classify(["ndrix"],"email","email").
classify(["ndrix","research"],_,"twenty").
classify(["ndrix","20"],_,"twenty").
classify(["ndrix","coincard"],_,"twenty").
classify(["jjgames"|_],_,"jjgames").
classify(["scs"|_],_,"scs").
classify(["vgpc"|_],_,"vgpc").
classify(_Ts,_M,U) :-
    U = "other".
    %format("unknown: %s|%s", [s(join_list(",",Ts)),s(M)], U).
