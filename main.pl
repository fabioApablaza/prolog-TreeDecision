:- use_module("./programs/csvReader.pl",[loadData/3,domain_of_attributes/3]). %getSample(DataSetPath, TrainingSamplePorcentage, TrainingSample,TestingSample, Header)

% We load the data
main(Domains,Records):-
    loadData('./DataSets/wineDatset.csv', Attributes, Records), domain_of_attributes(Attributes,Records, Domains).


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
