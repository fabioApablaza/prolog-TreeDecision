:- module(printSortedTree, [printSortedTree/1,saveTree/2]).

%Original https://www.rosettacode.org/wiki/Repeat_a_string#Prolog
repeat(Str,1,Str).
repeat(Str,Num,Res):-
    Num1 is Num-1,
    repeat(Str,Num1,Res1),
    string_concat(Str, Res1, Res).
getPadding(PaddingSeed,Pading):-
    PaddingLen is PaddingSeed * 3,
    repeat(' ',PaddingLen,Pading).

getRootDistance(root,_,0).
getRootDistance(NodeIndex,TreeNodes,RootDistance):-
    %write('getRootDistance'),
    Node=treeNode(NodeIndex,_,ParentIndex,_),
    member(Node,TreeNodes),

    getRootDistance(ParentIndex,TreeNodes,ParentRootDistance),
    RootDistance is ParentRootDistance+1.


printSortedTreeAux([],_).
printSortedTreeAux([CurrentNode|MoreNodes],TreeNodes):-
    
    CurrentNode=treeNode(CurrentNodeIndex,CurrentNodeLabel,_,NodeAction),
    
    getRootDistance(CurrentNodeIndex,TreeNodes,RootDistance),
    %write(RootDistance).%,
    getPadding(RootDistance,Padding),

    write(Padding),write(CurrentNodeLabel),write(' - '),write(NodeAction),nl,
    printSortedTreeAux(MoreNodes,TreeNodes).

printSortedTree(Tree):-!,
    write('root'),nl,!,
    printSortedTreeAux(Tree,Tree).



saveTreeAux(_OutStream,[]):-!.
saveTreeAux(OutStream,[TreeNode|MoreNodes]):-
    write(OutStream,TreeNode),write(OutStream,'.'),nl(OutStream),
    saveTreeAux(OutStream,MoreNodes).
saveTree(Tree,FileName):-
    % To read the file we must use consult(FilePath/FileName) and
    % then use findall(treeNode(NodeIndex,Label,Parent,Action),treeNode(NodeIndex,Label,Parent,Action),Tree)
    open(FileName,write,OutStream),
    findall(TreeNode,(
        member(TreeNode,Tree),
        write(OutStream,TreeNode),write(OutStream,'.'),nl(OutStream)
        ),Tree),close(OutStream).