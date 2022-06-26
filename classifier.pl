:- use_module("./programs/csvReader.pl",[loadData/3,domainOfAttributes/3]).
:- use_module("./programs/dataProcessing.pl",[discretizeColumn/4,dropColumns/5,processAndAssertRecords/5,cutElementFromListByIndex/3]). 

:-consult("./myTree.pl").

getTree(Tree):-
    findall(TreeNode,
        (
            treeNode(NodeIndex,NodeLabel,NodeParent,Action),
            TreeNode=treeNode(NodeIndex,NodeLabel,NodeParent,Action)
        ),Tree).


classifyRecordAux(AttributesValues,Tree,PredictedClassName,ParentIndex,PredictedClass):-
    member(treeNode(NodeIndex,Attribute=ExpectedValue,ParentIndex,Action),Tree),
    %write(treeNode(NodeIndex,Attribute=ExpectedValue,ParentIndex,Action)),nl,
    member(Attribute=ExpectedValue,AttributesValues),
    %write(Action),
    (
        Action=return(PredictedClassName=PredictedClass/_);
        classifyRecordAux(AttributesValues,Tree,PredictedClassName,NodeIndex,PredictedClass)
    ).

classifyRecord(PredictedClassName,RealClass,PredictedClass):-
    %processRecord
    getTree(Tree),
    %write(Tree),nl,
    processedRecord(RecordIndex,RealClass,ClassAttributes)=processedRecord(1,1,[hair=1,feathers=0,eggs=0,milk=1,airbone=0,aquatic=0,predator=1,toothed=1,backbone=1,breathes=1,venomous=0,fins=0,legs=4,tail=0,domestic=0,catsize=1]),
    %write(processedRecord(RecordIndex,RealClass,ClassAttributes)),nl,
    classifyRecordAux(ClassAttributes,Tree,PredictedClassName,root,PredictedClass).


classifyProccessedRecord(ProccessedRecord,Tree,PredictedClassName,RecordIndex,RealClass,PredictedClass):-
    ProccessedRecord=processedRecord(RecordIndex,RealClass,ClassAttributes),
    %write(RecordIndex),nl,
    classifyRecordAux(ClassAttributes,Tree,PredictedClassName,root,PredictedClass).%,
    %write(RecordIndex),write(' is '),write(PredictedClassName),write('='),write(RealClass),
    %write(', the tree says '),write(PredictedClassName),write('='),write(PredictedClass),nl.

classifyTrainingRecords([],_,_,[]).
classifyTrainingRecords([Record | Records],Tree,PredictedClassName,[NewClassifiedRecord| MoreClassifiedRecords]):-
    classifyProccessedRecord(Record,Tree,PredictedClassName,RecordIndex,RealClass,PredictedClass),
    NewClassifiedRecord = classifiedRecord(RecordIndex,real_class=RealClass,preddicted_class=PredictedClass),

    classifyTrainingRecords(Records,Tree,PredictedClassName,MoreClassifiedRecords).

testTree(Results,ColumnsToDrop):-
    getTree(Tree),
    loadData('./DataSets/testZoo.csv', Attributes, Records),
    %Discretization should be here
    discretizeColumn(Records,Attributes, NewRecords,NewAttributes),
    %write(Records),nl,
    %write(NewRecords),nl,
    dropColumns(NewRecords,NewAttributes,ColumnsToDrop,CuttedRecords,CuttedAttributes),
    
    length(CuttedAttributes,CuttedAttributesLen),
    TargetColumn is CuttedAttributesLen-1,
    processAndAssertRecords(CuttedRecords,CuttedAttributes,TargetColumn,ProccesedAttributes,ProccesedRecords),
    
    classifyTrainingRecords(ProccesedRecords,Tree,type,Results),write(Results),nl
    .
%    true.


%testTree(_,[0])