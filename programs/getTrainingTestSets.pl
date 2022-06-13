:- module(getSample, [getSample/5]).
:- use_module(library(csv)).

getSampleDataAux(TestingSample,0,[],TestingSample).

getSampleDataAux(DataSet,TrainingSampleLen,[RandomMember| RemainingTrainingSample],TestingSample):-
    random_member(RandomMember, DataSet),
    delete(DataSet, RandomMember, RemaingDataSet),
    RemainTrainingSampleLen is TrainingSampleLen-1,
    getSampleDataAux(RemaingDataSet,RemainTrainingSampleLen,RemainingTrainingSample,TestingSample)     
    .

getSampleData(DataSet,Header,TrainingSampleLen, [Header | TrainingSample ],[Header | TestingSample ]):-
    getSampleDataAux(DataSet,TrainingSampleLen,TrainingSample,TestingSample).

getSample(DataSetPath, TrainingSamplePorcentage, TrainingSample,TestingSample, Header):-
    TrainingSamplePorcentage<1, %Validation
    csv_read_file(DataSetPath, DataSet), % We get the dataset
    DataSet= [Header | Data],
    HeaderNames=..[row | Names], 
    length(Data, DataLength), % Number of records

    TrainingSampleLen is round(TrainingSamplePorcentage * DataLength),
    getSampleData(DataSet,Header,TrainingSampleLen,TrainingSample,TestingSample).
