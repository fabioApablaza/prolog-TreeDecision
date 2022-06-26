:-module(dataProcessing,[discretizeColumn/4,dropColumns/5,dropColumn/5,processAndAssertRecords/5,cutElementFromListByIndex/3,extractAttributeAsTarget/5]).

replace(Index, Record, NewElement, NewRecord) :-
    nth0(Index, Record, _, Transfer),
    nth0(Index, NewRecord, NewElement, Transfer).

discretizeColumnAux([],_,[]).
discretizeColumnAux([Record | MoreRecords],Index,[NewRecord|AccNewRecords]):-
    nth0(Index,Record,LegsValue),
    %write(Index),
    %write(LegsValue),
    (LegsValue > 2 -> NewElement is 1; NewElement is 0),
    %write(NewElement),nl,
    replace(Index, Record, NewElement, NewRecord),
    discretizeColumnAux(MoreRecords,Index,AccNewRecords).

%Attributes and Recors are list [type,bla, bla ,bla]
discretizeColumn(Records,Attributes, NewRecords,NewAttributes):-
    select(legs,Attributes,more_than_two_legs,NewAttributes),
    nth0(LegsIndex,Attributes,legs),
    discretizeColumnAux(Records,LegsIndex,NewRecords).



%Get the last element of a list
getElementByIndex([],_,[]).
getElementByIndex(List,Index,TargetElement):-
    nth0(Index,List,TargetElement).

%Extract an element from a list based in an index.
cutElementFromListByIndex([],_,[]).
cutElementFromListByIndex(List,Index,CuttedList):-
    nth0(Index,List,_,CuttedList).


dropColumnFromRecords([],_,[]).
dropColumnFromRecords([Record | MoreRecords],Index,[CuttedRecord | MoreCuttedRecords]):-
    nth0(Index,Record,AttributeToDrop),
    delete(Record,AttributeToDrop,CuttedRecord),
    dropColumnFromRecords(MoreRecords,Index,MoreCuttedRecords).

dropColumnsAux(CuttedRecords,CuttedAttributes,[],CuttedRecords,CuttedAttributes).
dropColumnsAux(Records,Attributes,[AttributeToDrop|MoreAttributesToDrop],CuttedRecords,CuttedAttributes):-
    % record come as row(attributes)
    nth0(Index,Attributes,AttributeToDrop),
    delete(Attributes,AttributeToDrop,PreCuttedAttributes),
    dropColumnFromRecords(Records,Index,PreCuttedRecords),
    dropColumnsAux(PreCuttedRecords,PreCuttedAttributes,MoreAttributesToDrop,CuttedRecords,CuttedAttributes).

dropColumns(Records,Attributes,ColumnsToDrop,CuttedRecords,CuttedAttributes):-
    %write(Attributes),
    findall(Attribute,(
        member(Index,ColumnsToDrop),
        nth0(Index,Attributes,Attribute)        
        ),AttributesToDrop),
    %write('Attribute to drop: '),write(AttributesToDrop),nl,
    dropColumnsAux(Records,Attributes,AttributesToDrop,CuttedRecords,CuttedAttributes).

dropColumn(Records,Attributes,ColumnIndex,CuttedRecords,CuttedAttributes):-
    cutElementFromListByIndex(Attributes,ColumnIndex,CuttedAttributes),
    length(Records,RecordsLength),

    % We create a list as large of Record list in which each element is equal to ColumnIndex 
    length(ColumnIndexList,RecordsLength),
    maplist(=(ColumnIndex),ColumnIndexList),
    
    maplist(cutElementFromListByIndex, Records,ColumnIndexList,CuttedRecords),
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

