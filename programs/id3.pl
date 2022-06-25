:- module(id3, [id3/3]).
:- use_module("./programs/entropy.pl",[entropy/3,conditionalEntropy/3]).

classCount([Class | MoreClasses], Records, [Class/ClassOccurencies| MoreAccounts]):-
    %Extract record with the Class we are looking for
    findall(Record,(member(Record,Records),Record=processedRecord(_,Class,_)),RecordsOfClass),
    length(RecordsOfClass,ClassOccurencies),
    classCount(MoreClasses,Records,MoreAccounts).

distribution(Records,RecordDistribution):-
    %Extract class values from records
    setof(Class,(
        member(Record,Records),
        Record=processedRecord(_,Class,_)),
    Classes),
    classCount(Classes,Records,RecordDistribution).

% All records are from the same class
induct(Records,Parent,_,_,AcNodes,[treeNode(leaf,[Class],Parent)|AcNodes]):-
    distribution(Records,[Class]).


getAttributeValues(Records,Attribute,Values):-
    setof(Value,(Record,Records,Index,Attributes,Class,Attribute)^(
        member(Record,Records),
        Record=processedRecord(Index,Class,Attributes),
        member(Attribute=Value,Attributes)),    
    Values).

getPartitionByAttributeValue([],_,[]):-!.
getPartitionByAttributeValue([Record | MoreRecords],AttributeValue,[Record| MoreRecordsForCurrentPartiton]):-
    Record=processedRecord(_,_,RecordAttributesValues),
    member(AttributeValue,RecordAttributesValues),!,
    getPartitionByAttributeValue(MoreRecords,AttributeValue,MoreRecordsForCurrentPartiton).

getPartitionByAttributeValue([_|MoreRecords],AttributeValue,RecordsForCurrentPartiton):-
    getPartitionByAttributeValue(MoreRecords,AttributeValue,RecordsForCurrentPartiton).

getPartitionsByAttribute([],_,_,[]):-!.
getPartitionsByAttribute([AttributeValue|MoreAttributesValues],Records,Attribute,[Partition|MorePartitions]):-
    getPartitionByAttributeValue(Records,Attribute=AttributeValue,Partition),
    getPartitionsByAttribute(MoreAttributesValues,Records,Attribute,MorePartitions).

getMaxInformationGain([X],X):-!.
getMaxInformationGain([Value/InformationGain | MoreInformationGains],MaxValue/MaxGain):
    getMaxInformationGain(MoreInformationGains, AuxMaxValue/AuxMaxGain),
    (
        InformationGain>AuxMaxGain,MaxValue/MaxGain =Value/InformationGain;
        MaxValue/MaxGain = AuxMaxValue/AuxMaxGain
    ).

eliminateElementFromList(X,[X|T], T):-!.
eliminateElementFromList(X,[Y|T], [Y|Z]):- eliminateElementFromList(X,T,Z).


chooseAttribute(Records,Classes,Attributes,Attribute,Values,OtherAttributes):-
    writeln('Attributes: '),writeln(Attributes),nl,
    length(Records, RecordsLen),
    entropy(Records,RecordsLen,Entropy),
    findall((Attribute-AttributeValues)/InformationGain,(
        member(Attribute, Attributes),
        getAttributeValues(Records,Attribute,AttributeValues),
        getPartitionsByAttribute(AttributeValues,Records,Attribute,Partitions),
        conditionalEntropy(Partitions,RecordsLen,ConditionedEntropy),
        InformationGain is Entropy-conditionedEntropy
    ),InformationGains),
    writeln('Information gains: '),writeln(InformationGains),nl.
    %getMaxInformationGain(InformationGains,(Attribute/Values)/_),
    %eliminateElementFromList(Attribute,Attributes,OtherAttributes).


%induct(Records,Parent,_,_,AcNodes,[treeNode(leaf,[Class],Parent)|AcNodes]):-
particionate([],_,_,_,_,Tree,Tree):-!.
particionate([AttributeValue|MoreAttributeValues],Attribute, Records,Parent,OtherAttributes,AcNodes,Tree):-
    getPartitionByAttributeValue(Records, Attribute=AttributeValue,Partition),
    length(AcNodes,NewId),
    NewNode=treeNode(NewId,Attribute=AttributeValue,Parent),
    NewAccNodes=[NewNode|AcNodes],
    
    %To correct
    induct(Partition,NewNode,[],OtherAttributes,NewAccNodes,PreTree),!,
    particionate(MoreAttributeValues,Attribute,Records,Parent,OtherAttributes,PreTree,Tree).

% We must choose which attribute is more relevant to clasificate the given records
induct(Records, Parent,Classes,Attributes,AcNodes,Tree):-
    write('Inducting'),nl,
    chooseAttribute(Records,Classes,Attributes,BestAttribute,BestAttributeValues,OtherAttributes),!.%,
    %particionate(BestAttributeValues,BestAttribute,Records,Parent,OtherAttributes,AcNodes,Tree).


id3(Records,ProccesedAttributes,Tree):-
    % We get unique values of classes
    setof(Class,(Index,Record,Records,Attributes)^(
        member(Record,Records),
        Record=processedRecord(Index,Class,Attributes)),
    Classes),
    

    Tree=true,
    ProccesedAttributes=[Attribute | _],
    getAttributeValues(Records, Attribute,AttributeValues),

    length(Records,RecordsLen),
    getPartitionsByAttribute(AttributeValues,Records,Attribute,Partitions),
    conditionalEntropy(Partitions,RecordsLen,ConditionalEntropy),
    entropy(Records,RecordsLen,Entropy),
    InformationGain is Entropy-ConditionalEntropy,
    
    write('Entropy: '),write(Entropy),nl,
    write('Conditional Entropy: '),write(ConditionalEntropy),nl,
    write('Information gain: '),write(InformationGain),nl.
    %induct(Records,root,Classes,ProccesedAttributes,[],Tree).




% Get breakpoints
% Preclassify attributes in numerical and acategorical, order indexes by numerical attributes, in each iteration decide wether the attributes is numerical or categorical, if categorical use what it's already done, if not calculate break points and select the one with better distribution (tip: decide from the middle, option2: calculate the one with best entropy)

%ID3 continous values https://www.youtube.com/watch?v=2vIvM4zmyf4
% https://www.naun.org/main/NAUN/mcs/17-213.pdf