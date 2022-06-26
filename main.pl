
:- use_module("./programs/id3.pl",[id3/3]).
:- use_module("./programs/csvReader.pl",[loadData/3,domainOfAttributes/3]).
:- use_module("./programs/dataProcessing.pl",[dropColumns/5,discretizeColumn/4,dropColumn/5,processAndAssertRecords/5,cutElementFromListByIndex/3]). 


discretizeAttribute(Attribute,AttributeColumn).

% We load the data
processZooData(Domains,ProccesedAttributes,ProccesedRecords,Tree):-
    loadData('./DataSets/trainingZoo.csv', Attributes, Records),
    discretizeColumn(Records,Attributes, NewRecords,NewAttributes),

    %write(NewAttributes),nl,
    %write(NewRecords),nl,
    dropColumns(NewRecords,NewAttributes,[0],CuttedRecords,CuttedAttributes),
    write('Cutted attributes length: '),write(CuttedAttributes),nl,
    %dropColumn(Records,Attributes,0,CuttedRecords,CuttedAttributes),
    domainOfAttributes(CuttedAttributes,CuttedRecords, Domains),
    length(CuttedAttributes,CuttedAttributesLen),
    TargetColumn is CuttedAttributesLen-1,
    processAndAssertRecords(CuttedRecords,CuttedAttributes,TargetColumn,ProccesedAttributes,ProccesedRecords),
    id3(ProccesedRecords,ProccesedAttributes,Tree).



main(Domains,ProccesedAttributes,ProccesedRecords,Tree):-
    loadData('./DataSets/zooDataset.csv', Attributes, Records),
    
    %dropColumn(Records,Attributes,13,CuttedRecords,CuttedAttributes),
    domainOfAttributes(Attributes,Records, Domains),
    ProccesedRecords=Attributes,
    ProccesedAttributes=Records,
    %processAndAssertRecords(CuttedRecords,CuttedAttributes,0,ProccesedAttributes,ProccesedRecords),
    %id3(CuttedRecords,Tree).
    Tree=true.


