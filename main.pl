:- use_module("./programs/csvReader.pl",[loadData/3,domain_of_attributes/3]).
:- use_module("./programs/dataProcessing.pl",[dropColumn/5,processAndAssertRecords/5,cutElementFromListByIndex/3]). 

% We load the data
main(Domains,ProccesedAttributes,ProccesedRecords):-
    loadData('./DataSets/wineDatset.csv', Attributes, Records),
    
    dropColumn(Records,Attributes,13,CuttedRecords,CuttedAttributes),
    domain_of_attributes(CuttedAttributes,CuttedRecords, Domains),
    %dropColumn(Records,Attributes,12,_,CuttedAttributes),
    processAndAssertRecords(CuttedRecords,CuttedAttributes,0,ProccesedAttributes,ProccesedRecords),
    %processAndAssertRecords(Records,Attributes,0,ProccesedAttributes,ProccesedRecords),
    
    true.



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
