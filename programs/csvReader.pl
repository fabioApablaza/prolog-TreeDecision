% Material original https://www.uv.mx/personal/aguerra/files/2020/09/pia-03.pdf ; Sección 3.5.5
:- module(loadData, [loadData/3,domainOfAttributes/3]).
:- use_module(library(csv)).

getRowAttributesValue(Row,RowArguments):-
    Row=..[_|RowArguments]. % Operator =.. : X =..[predicate | PredicateArguments] <=> X = predicate(PredicateArguments)

loadData(CSV_file, Attributes, RecordsAsListOfList):-
    csv_read_file(CSV_file,[Header | RecordsAsRows], [strip(true)]), %strip(true) eliminate white spaces
    Header=..[_| Attributes],
    %We get a list of the attributes of each record
    maplist(getRowAttributesValue, RecordsAsRows,RecordsAsListOfList), %maplist(predicate, ListOfListOfPredicatesArguments1, ListOfListOfPredicatesArguments2,...,ListOfListOfPredicatesArgumentsN)
    length(RecordsAsListOfList,RecordNum),
    write(RecordNum), write(' records loaded.'), nl.


% Return all the values for the records in the position Index as a List
column_unique_values(Index,Records,Values):-
    %Qye hace nth0?
    maplist(nth0(Index), Records, Values).


%Given a list of records an its arguments, it calculates the domains of each argument.
domainOfAttributes(Attributes,Records, Domains):-
    findall([Attribute,Domain], ( % Get every attribute and its domain
        member(Attribute, Attributes), % Get an attribute from attributes
        nth0(Index, Attributes, Attribute), % Get the Attribute index in Attributes List
        column_unique_values(Index,Records,DomainAsList),
        list_to_set(DomainAsList, Domain)
    ), Domains)
    .

