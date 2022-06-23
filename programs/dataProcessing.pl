:-module(dataProcessing,[dropColumn/5,processAndAssertRecords/5,cutElementFromListByIndex/3,extractAttributeAsTarget/5]).

%Get the last element of a list
getElementByIndex([],_,[]).
getElementByIndex(List,Index,TargetElement):-
    nth0(Index,List,TargetElement).

%Extract an element from a list based in an index.
cutElementFromListByIndex([],_,[]).
cutElementFromListByIndex(List,Index,CuttedList):-
    nth0(Index,List,_,CuttedList).

dropColumn(Records,Attributes,ColumnIndex,CuttedRecords,CuttedAttributes):-
    cutElementFromListByIndex(Attributes,ColumnIndex,CuttedAttributes),
    length(Records,RecordsLength),

    % We create a list as large of Record list in which each element is equal to ColumnIndex 
    length(ColumnIndexList,RecordsLength),
    maplist(=(ColumnIndex),ColumnIndexList),
    
    maplist(cutElementFromListByIndex, Records,ColumnIndexListd,CuttedRecords),
    true.

%The last element put an atributes equal to its atribute value
makeAttributeLegible(Attribute,AttributeValue,Attribute=AttributeValue).

extractAttributeAsTargetAux(_,_,_,[],[]).
extractAttributeAsTargetAux(RecordIndex,AttributesNamesWithoutTargetAttribute,TargetColumn, [CurrentRecordAttributes|MoreRecordsAttributes], [processedRecord(RecordIndex,Target,LegibleAttributesValuesToKnow)|MoreProcessedRecords]):-
    getElementByIndex(CurrentRecordAttributes,TargetColumn,Target), %We get the target value from the record
    cutElementFromListByIndex(CurrentRecordAttributes,TargetColumn,AttributesValuesToKnow), %We cut the target value from the record
    maplist(makeAttributeLegible,AttributesNamesWithoutTargetAttribute,AttributesValuesToKnow,LegibleAttributesValuesToKnow), % We make the record attributes more legible
    NextRecordIndex is  RecordIndex+1, % We update the index
    extractAttributeAsTargetAux(NextRecordIndex,AttributesNamesWithoutTargetAttribute,TargetColumn,MoreRecordsAttributes,MoreProcessedRecords). % We analize more records


extractAttributeAsTarget(Records,Attributes,TargetAttributeIndex,ProccesedAttributes,ProccesedRecords):-
    cutElementFromListByIndex(Attributes,TargetAttributeIndex,ProccesedAttributes),
    extractAttributeAsTargetAux(1,ProccesedAttributes, TargetAttributeIndex, Records,ProccesedRecords).

processAndAssertRecords(Records,Attributes,TargetAttributeIndex,ProccesedAttributes,ProccesedRecords):-
    extractAttributeAsTarget(Records,Attributes,TargetAttributeIndex,ProccesedAttributes,ProccesedRecords),
    maplist(assertz,ProccesedRecords),
    true.

