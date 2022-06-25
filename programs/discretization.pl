% Entropy-based Binning
% Main idea: https://www.youtube.com/watch?v=gmiINKkYcF8

orderElementByAttributeAux(RecordIndex,AttributeValue,Records,[OrderedRecordIndex|MoreOrderedRecordIndex],NewOrderedRecordsIndexes):-
    member(processedRecord(OrderedRecordIndex,_,OrderedRecordAttributesValues),Records),
    member(Attribute=OrderedAttributeValue,OrderedRecordAttributesValues),
    ((
        AttributeValue>OrderedAttributeValue,
        orderElementByAttributeAux(RecordIndex,AttributeValue,Records,MoreOrderedRecordIndex,NewOrderedRecordsIndexes)
    );
        OldOrderedRecordIndexes=[OrderedRecordIndex|MoreOrderedRecordIndex],
        NewOrderedRecordsIndexes=[RecordIndex|OldOrderedRecordIndexes]
    ).
orderElementByAttribute([],_,_,OrderedRecord).
orderElementByAttribute([Record|MoreRecords],Records,Attribute,[],OrderedRecords):-
    orderElementByAttribute(MoreRecords,Records,Attribute,[Record],OrderedRecords).
orderElementByAttribute([Record|MoreRecords],Records,Attribute,AuxOrderedRecord,OrderedRecords):-
    Record=processedRecord(Index,Class,RecordAttributesValues),
    member(Attribute=AttributeValue,RecordAttributesValues),
    orderElementByAttribute(MoreRecords,Attribute,[],OrderedRecords).
%


getBreakpoints([],_,_,_,Breakpoints,Breakpoints).
getBreakpoints([RecordIndex|MoreRecordIndexes],Attribute,Records,initial,[],Breakpoints):-
    %Initial getBreakpoint Metod to call
    member(processedRecord(RecordIndex,Class,RecordAttributesValues),Records),
    getBreakpoints(MoreRecordIndexes,Attribute,Records,processedRecord(RecordIndex,Class,RecordAttributesValues),[],Breakpoints),
    true.
%

getBreakpoints([RecordIndex|MoreRecordIndexes],_,Records,processedRecord(_,Class,_),AccBreakpoint,Breakpoints):-
    %If there is not a breakpoint
    member(processedRecord(RecordIndex,Class,RecordAttributesValues),Records),
    getBreakpoints(MoreRecordIndexes,Attribute,Records,processedRecord(RecordIndex,Class,RecordAttributesValues),AccBreakpoint,Breakpoints).
%
getBreakpoints([RecordIndex|MoreRecordIndexes],Attribute,Records,processedRecord(PreviousRecordIndex,PreviousClass,PreviousRecordAttributesValues),AccBreakpoint,Breakpoints):-
    member(processedRecord(RecordIndex,Class,RecordAttributesValues),Records),
    Class =\=PreviousClass,
    member(Attribute=CurrentRecordValue,RecordAttributesValues),
    member(Attribute=PrevioustRecordValue,PreviousRecordAttributesValues),
    BreakValue is (CurrentRecordValue + PrevioustRecordValue)/2,
    NewBreakPoint=breakpoint(Attribute,PreviousRecordIndex,RecordIndex,BreakValue),
    %The new breakpoint should be added las to matain the breakpoints order
    append(AccBreakpoint,[NewBreakPoint],NewAccBreakpoints),
    getBreakpoints(MoreRecordIndexes,Attribute,Records,processedRecord(RecordIndex,Class,RecordAttributesValues),NewAccBreakpoints,Breakpoints).
%


orderBreakPointsAux(Breakpoint,OrderedBreakpoints,[BreakPoint|OrderedBreakpoints]):-
    OrderedBreakpoints=[OrderedBreakpoint|MoreOrderedBreakpoints],
    BreakPoint=breakpoint(Attribute,CurrentLowerRecordIndex,CurrentUpperRecordIndex,CurrentBreakValue),
    OrderedBreakpoint=breakpoint(Attribute,OrderedLowerRecordIndex,OrderedUpperRecordIndex,OrderedBreakValue),
    CurrentBreakValue<OrderedBreakValue.
%

orderBreakPointsAux(Breakpoint,OrderedBreakpoints,NewOrderedBreakpoints):-
    OrderedBreakpoints=[OrderedBreakpoint|MoreOrderedBreakpoints],
    BreakPoint=breakpoint(Attribute,CurrentLowerRecordIndex,CurrentUpperRecordIndex,CurrentBreakValue),
    OrderedBreakpoint=breakpoint(Attribute,OrderedLowerRecordIndex,OrderedUpperRecordIndex,OrderedBreakValue),
    orderBreakPointsAux(Breakpoint,MoreOrderedBreakpoints,PreNewOrderedBreakpoints),
    NewOrderedBreakpoints=[OrderedBreakpoint|PreNewOrderedBreakpoints].
%


orderBreakPoints([],OrderedBreakpoints,OrderedBreakpoints).

orderBreakPoints([BreakPoint| MoreBreakpoints],[],OrderedBreakpoints):-
    orderBreakPoints(MoreBreakpoints,[BreakPoint],OrderedBreakpoints).
%

orderBreakPoints([BreakPoint| MoreBreakpoints],AccOrderedBreakPoints,OrderedBreakpoints):-
    BreakPoint=breakpoint(Attribute,CurrentLowerRecordIndex,CurrentUpperRecordIndex,CurrentBreakValue),
    orderBreakPointsAux(BreakPoint,AccOrderedBreakPoints,NewAccOrderedBreakPoints),
    orderBreakPoints(MoreBreakpoints,NewAccOrderedBreakPoints,OrderedBreakpoints).
%

getValidatedBreakPoint(BreakPoints,BreakPointsLen,PendingBreakPointsToAdd,ValidBreakPoint):-
    member(ValidBreakPoint,Breakpoints),
    nth0(ValidBreakpointIndex,Breakpoints,ValidBreakPoint),
    (ValidBreakpointIndex+PendingBreakPointsToAdd) < BreakPointsLen.
%

getUpperBreakPoint(CurrentBreakPoint,Breakpoints,UpperBreakpoint):-
    member(UpperBreakpoint,Breakpoints),
    CurrentBreakPoint=breakpoint(_,_,_,CurrentBreakValue),
    UpperBreakpoint=breakpoint(_,_,_,UpperBreakValue),
    CurrentBreakValue < UpperBreakValue.
%

getSubPartitionByBreakpoints(BreakPoints,1,[Breakpoint]):-
    member(Breakpoint,BreakPoints).
%

getSubPartitionByBreakpoints(BreakPoints,PartitonTargetLen,SubPartition):-
    length(Breakpoints,BreakPointsLen),
    PendingBreakPointsToAdd is PartitonTargetLen-1,
    getValidatedBreakPoint(BreakPoints,BreakPointsLen,PendingBreakPointsToAdd,ValidBreakPoint),
    getUpperBreakPoint(ValidBreakPoint,Breakpoints,UpperBreakpoint),
    getSubPartitionByBreakpoints(UpperBreakpoint,PendingBreakPointsToAdd,AuxSubPartition),
    SubPartition=[Breakpoint|AuxSubPartition].
%

getPartitionsByBreakpoints(Breakpoints,1,Breakpoints).
getPartitionsByBreakpoints(Breakpoints,PartitonTargetLen,Partitions):-
    %This method generate all possible partitions between breakpoint of size of 2 to MaxNumberOfPartitions
    %This generatedPartitions are ordered in an asscending order
    PendingBreakPointsToAdd is PartitonTargetLen-1,
    findall(Partition,(
        getValidatedBreakPoint(BreakPoints,BreakPointsLen,PendingBreakPointsToAdd,ValidBreakPoint),
        getUpperBreakPoint(ValidBreakPoint,Breakpoints,UpperBreakpoint),
        getSubPartitionByBreakpoints(UpperBreakpoint,PendingBreakPointsToAdd,SubPartition),
        Partition=[ValidBreakPoint|SubPartition]
        ),Partitions).
%

entropyBasedBinning(ProccessedRecords,TargetAttribute,Categories):-
    
    true.




discretize(ProccessedRecords, Attributes, AttributesTyp,NewProccessedRecords):-
        
    true.