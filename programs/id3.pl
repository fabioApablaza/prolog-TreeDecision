:- module(id3, [id3/3]).

% Probabilidad de clase

discretEntropyTypeAux([],Entropy, Entropy).

discretEntropyType([(DiscreteValue, Probability)| MoreDiscreteValueInfo],Entropy, AcEntropy):- 
    TermValue is -1 * Probability * log(Probability)/log(2),
    NewAcEntropy is AcEntropy+TermValue,
    discretEntropyTypeAux(MoreDiscreteValueInfo,Entropy, NewAcEntropy).


discretEntropy(DiscreteValueInfo,Entropy):-
    
    discretEntropyAux(DiscreatValueInfo,Entropy,0).

getClasses(TrainingSet,ClassColumn,Classes):-true.


id3(TrainingSet, ClassColumn,Classes):-
    % We get unique values of the class column
    getClasses(TrainingSet,ClassColumn,Classes).