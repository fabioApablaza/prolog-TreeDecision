:- module(entropy, [entropy/3,conditionalEntropy/3]).

entropy([],_,_,0):-!.
entropy([Class | Classes], Records, RecordsNum, Sum):-

    %Find all records of a class in the partition
    findall(Record,(member(Record,Records), Record=processedRecord(_,Class,_)),RecordsInClass),
    length(RecordsInClass, ClassOccurrencies),

    %Conditional probability
    Probability is ClassOccurrencies/RecordsNum,
    entropy(Classes,Records,RecordsNum,SumAux), 
    Sum is SumAux - Probability * log(Probability)/log(2).    

entropy(Records,RecordsNum,Entropy):-
    %Extract class values from records
    setof(Class,(Index,Record,Records,Attributes)^(
        member(Record,Records),
        Record=processedRecord(Index,Class,Attributes)),
    Classes),!,
    
    %Classes depending on partition, Partition element, Partition len, result
    entropy(Classes, Records, RecordsNum,Entropy).



calculateConditionalEntropyTermAux([],_,_,_,0).
calculateConditionalEntropyTermAux([Class|Classes], Partition,PartitionLen,RecordsLen,EntropyNumeratorTerm):-
    %Find all records of a class in the partition
    findall(Record,(member(Record,Partition), Record=processedRecord(_,Class,_)),RecordsInClass),
    length(RecordsInClass, ClassOccurrencies),

    %Conditional probability
    Probability is ClassOccurrencies/PartitionLen,
    calculateConditionalEntropyTermAux(Classes,Partition,PartitionLen,RecordsLen,SumAux), 
    EntropyNumeratorTerm is SumAux - (ClassOccurrencies * log(Probability)/log(2))/RecordsLen.
%


calculateConditionalEntropyTerm(Partition,PartitionLen,RecordsLen,EntropyNumeratorTerm):-
    %Extract class values from records
    setof(Class,(Index,Record,Partition,Attributes)^(
        member(Record,Partition),
        Record=processedRecord(Index,Class,Attributes)),
    Classes),!,    
    %Classes depending on partition, Partition element, Partition len, result
    
    calculateConditionalEntropyTermAux(Classes, Partition,PartitionLen,RecordsLen,EntropyNumeratorTerm).
%

conditionalEntropy([],_,0):-!.
conditionalEntropy([Partition | MorePartitions],RecordsLen,ConditionalEntropy):-
    length(Partition, PartitionLen),

    calculateConditionalEntropyTerm(Partition,PartitionLen,RecordsLen,EntropyNumeratorTerm),
    conditionalEntropy(MorePartitions,RecordsLen,AcConditionalEntropy),
    %ConditionalEntropy is EntropyNumeratorTerm.
    ConditionalEntropy is AcConditionalEntropy+ EntropyNumeratorTerm.