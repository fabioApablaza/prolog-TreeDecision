%% :- use_module(library(csv)).

%% tree(N, Nil, Nil).
%% tree(N, tree(N, M, X), tree(N, M, X)).


%% csv_read_file('DataSets/wineDatset.csv', Rows, []).

id3(Examples, T, A):-
    t(B, Izq, Der),
    (
    X > 1 ->
        writeln('as');
        writeln('m')
        ).
