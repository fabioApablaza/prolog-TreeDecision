%% :- use_module(library(csv)).

:- use_module("./programs/getTrainingTestSets.pl",[getSample/5]).

%% tree(N, Nil, Nil).
%% tree(N, tree(N, M, X), tree(N, M, X)).


%% csv_read_file('DataSets/wineDatset.csv', Rows, []).

%getSample('./DataSets/wineDatset.csv', 0.8, TrainingSampleWine,TestingSampleWine, HeaderWine).
%getSample('./DataSets/zooDatset.csv', 0.8, TrainingSampleZoo,TestingSampleZoo, HeaderZoo).

%id3(Examples, T, A):-
%    t(B, Izq, Der),
%    (
%    X > 1 ->
%        writeln('as');
%        writeln('m')
%        ).
