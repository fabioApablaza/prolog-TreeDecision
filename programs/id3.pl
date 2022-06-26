:- module(id3, [id3/3]).
:- use_module("./programs/entropy.pl",[entropy/3,conditionalEntropy/4]).
:- use_module("./programs/tree.pl",[printSortedTree/1,saveTree/2]).

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
induct(Records,Parent,_,_,AcNodes,[treeNode(leaf,[Class],Parent,return(Class))|AcNodes]):-
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

getMaxInformationGain([],(strange-[strange])/0.0). % -write('Case base'),nl.
getMaxInformationGain([(Attribute-Values)/InformationGain | MoreInformationGains],(BestAttribute-BestAttributeValues)/BestAttrbiuteInformationGain):-
    %write('getMaxInformationGain '),write(Attribute),nl,
    getMaxInformationGain(MoreInformationGains, (AuxBestAttribute-AuxBestAttributeValues)/AuxBestAttributeInformationGain),
    (((InformationGain>AuxBestAttributeInformationGain)->(BestAttribute=Attribute, BestAttributeValues=Values,BestAttrbiuteInformationGain=InformationGain));
        (BestAttribute=AuxBestAttribute, BestAttributeValues=AuxBestAttributeValues,BestAttrbiuteInformationGain=AuxBestAttributeInformationGain)).
%

chooseAttribute(Records,Classes,Attributes,BestAttribute,BestAttributeValues,OtherAttributes):-
    
    length(Records, RecordsLen),
    entropy(Records,RecordsLen,Entropy),
    findall((Attribute-AttributeValues)/InformationGain,(
        member(Attribute, Attributes),
        
        getAttributeValues(Records,Attribute,AttributeValues),
        
        getPartitionsByAttribute(AttributeValues,Records,Attribute,Partitions),
        length(Partitions, PartitionsLen),
        %nl,write(Attribute),nl,
        %write(Attribute),write(' number of partitions: '),write(PartitionsLen),nl,
        conditionalEntropy(Partitions,Attribute,RecordsLen,ConditionalEntropy),
        InformationGain is Entropy-ConditionalEntropy%,
        
        
        %write('Analizing attribute '),write(Attribute),nl,
        %write(Attribute),write(' conditional entropy: '),write(ConditionalEntropy),nl,
        %write(Attribute),write(' values: '),write(AttributeValues),nl
        %write(Attribute),write(' information gain: '),write(InformationGain),nl,nl

    ),InformationGains),!,
    getMaxInformationGain(InformationGains,(BestAttribute-BestAttributeValues)/BestAttributeInformationGain),
    % write('Max information gainer: '),write(BestAttribute),write('/'),write(BestAttributeValues),nl,
    % write(BestAttribute),write(' information gain: '),write(BestAttributeInformationGain),nl.
    % write('Attributes: '),writeln(Attributes),nl,
    % write('Best Attributes: '),write(BestAttribute),nl,
    delete(Attributes,BestAttribute,OtherAttributes).%,
    % write('Other Attributes: '),writeln(OtherAttributes),nl.



%induct(Records,Parent,_,_,AcNodes,[treeNode(leaf,[Class],Parent)|AcNodes]):-
getNewNodeAction(NewId,Partition,PartitionClassesLen,NodeAction):-
    ((PartitionClassesLen = 1) ->(
            Partition=[processedRecord(_,Class,_)| _],
            length(Partition,PartitionLen),
            NodeAction = return(type=Class/PartitionLen)
        ));
    NodeAction = checkSons.
getNewNodeAction(NewId,_,_,_):-write('Failing'),write(NewId),nl,false.

particionate([],_,_,_,_,Tree,Tree):-!.
particionate([AttributeValue|MoreAttributeValues],Attribute, Records,Parent,OtherAttributes,AcNodes,Tree):-
    ((Parent=root -> ParentId=root);(
        Parent=treeNode(ParentId,_,_,_)
    )),

    %writeln('Particionating'),
    getPartitionByAttributeValue(Records, Attribute=AttributeValue,Partition),
    length(AcNodes,NewId),
    NewNode=treeNode(NewId,Attribute=AttributeValue,ParentId,NodeAction),
    append(AcNodes,[NewNode],NewAccNodes),

    %Target Classes in this partition
    setof(Class,(Index,Record,Records,Attributes)^(
        member(Record,Partition),
        Record=processedRecord(Index,Class,Attributes)),
    Classes),
    length(Classes,PartitionClassesLen),
    getNewNodeAction(NewId,Partition,PartitionClassesLen,NodeAction),
    
    
    write(NewNode),nl,
    
    (((PartitionClassesLen =\= 1) -> (
        induct(Partition,NewNode,Classes,OtherAttributes,NewAccNodes,PreTree),
        particionate(MoreAttributeValues,Attribute,Records,Parent,OtherAttributes,PreTree,Tree)
    ));particionate(MoreAttributeValues,Attribute,Records,Parent,OtherAttributes,NewAccNodes,Tree)).

% We must choose which attribute is more relevant to clasificate the given records
induct(Records, Parent,Classes,Attributes,AcNodes,Tree):-
    %write('Inducting'),nl,
    chooseAttribute(Records,Classes,Attributes,BestAttribute,BestAttributeValues,OtherAttributes),
    %write('Best attribute: '),write(BestAttribute),write(' with values: '),write(BestAttributeValues),nl.%,
    particionate(BestAttributeValues,BestAttribute,Records,Parent,OtherAttributes,AcNodes,Tree).

id3(Records,ProccesedAttributes,Tree):-
    % We get unique values of classes
    setof(Class,(Index,Record,Records,Attributes)^(
        member(Record,Records),
        Record=processedRecord(Index,Class,Attributes)),
    Classes),
    
    induct(Records,root,Classes,ProccesedAttributes,[],Tree),
    printSortedTree(Tree),
    saveTree(Tree,'myTree.pl').%,
    %Records=[ProcessedRecord|_],
    %writeln(ProcessedRecord).



% Get breakpoints
% Preclassify attributes in numerical and acategorical, order indexes by numerical attributes, in each iteration decide wether the attributes is numerical or categorical, if categorical use what it's already done, if not calculate break points and select the one with better distribution (tip: decide from the middle, option2: calculate the one with best entropy)

%ID3 continous values https://www.youtube.com/watch?v=2vIvM4zmyf4
% https://www.naun.org/main/NAUN/mcs/17-213.pdf
% main(_,_,_,_)
% processZooData(_,_,_,_).