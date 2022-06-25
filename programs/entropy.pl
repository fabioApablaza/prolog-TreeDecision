:- module(entropy, [entropy/3,conditionedEntropy/3]).

entropy([],_,_,0):-!.
entropy([Class | Classes], Records, RecordsNum, Sum):-
    findall(Record,(member(Record,Records), Record=processedRecord(_,Class,_)),RecordsInClass),
    length(RecordsInClass, ClassOccurrencies),
    Probability is ClassOccurrencies/RecordsNum,
    entropy(Classes,Records,RecordsNum,SumAux), 
    Sum is SumAux - Probability * log(Probability)/log(2).    

entropy(Records,RecordsNum,Entropy):-
    %Extract class values from records
    setof(Class,(Index,Record,Records,Attributes)^(
        member(Record,Records),
        Record=processedRecord(Index,Class,Attributes)),
    Classes),!,

    entropy(Classes, Records, RecordsNum,Entropy).

conditionedEntropy([],_,0):-!.
conditionedEntropy([Partition | MorePartitions],RecordsLen,ConditionedEntropy):-
    length(Partition, PartitionLen),
    entropy(Partition,PartitionLen, PartitionEntropy),
    conditionedEntropy(MorePartitions,RecordsLen,AcPartitionsEntropy),
    ConditionedEntropy is AcPartitionsEntropy+ PartitionEntropy.