let
    fn = ( EditedTable as table, ListColumns as list ) as table =>
        let
            CorrectListStructure = not List.MatchesAny(
                List.Transform(
                    ListColumns,
                    each List.Count(_)
                ), each _ < 2 or _ > 3
            ),

            GenerateColumn = ()=> 
                if CorrectListStructure then
                    List.Last(
                        List.Generate(
                            () => [
                                TableRef = Table.Buffer( EditedTable ),
                                NewColumns = List.Buffer( ListColumns ),
                                Counter = 0,
                                NumberOfColumns = List.Count( ListColumns )
                            ],
                            each [Counter] <= [NumberOfColumns],
                            each [
                                NewColumns = [NewColumns],
                                Counter = [Counter] + 1,
                                NumberOfColumns = [NumberOfColumns],
                                TableRef = Table.AddColumn(
                                    [TableRef],
                                    [NewColumns]{[Counter]}{0},
                                    [NewColumns]{[Counter]}{1},
                                    if List.Count( [NewColumns]{[Counter]} ) = 3
                                        then [NewColumns]{[Counter]}{2}
                                        else Any.Type
                                )
                            ],
                            each [TableRef]
                        )
                    )
                else
                    error "#(lf)O parâmetro 'ListColumns' não está no formato esperado. A lista deve possuir para cada item:#(lf,tab)" &
                            "1. Ou apenas dois itens, sendo o primeiro o nome da nova coluna e a função que será aplicada;#(lf,tab)" & 
                            "2. Ou apenas três itens, adicionando no terceiro item o tipo de dados da coluna",
            Result = GenerateColumn()
        in
            Result,
    fnType = type function(
        EditedTable as table, 
        ListColumns as list 
    ) as table meta [
        Documentation.Name = "fnTableAddColumns" ,
        Documentation.LongDescription = 
            "Desc. PT-BR: Adiciona uma coluna com o nome, função e tipo de dados especificado em uma lista. Na lista deve conter para cada item, uma lista com o nome da coluna, a função que será operada e (opcional) tipo de dados da coluna, respectivamente.#(lf)" &
            "Desc. EN: Adds a column with the specified name, function, and data type to a list. The list must contain, for each item, a list with the name of the column, the function that will be operated on and (optional) the column's data type, respectively.",
        Documentation.Examples = {
            [
                Description = "Esta função adiciona duas colunas, no qual a primeira conta a quantidade de caracteres da coluna [Nome] e a segunda retorna se a pessoa possui menos de 18 anos.",
                Code = "fnTableAddColumns(
    Table.FromRecords({
        [Nome = ""Peter"", Idade = 15],
        [Nome = ""Roberta"", Idade = 19],
        [Nome = ""Paul"", Idade = 18],
        [Nome = ""Maria"", Idade = 17]
    }),
    {
        {""NumCaracteres"", each Text.Length([Nome]) },
        {""MenorDeIdade"",  each [Idade] < 18}
    }
)",
                Result = "Table.FromRecords({
        [Nome = ""Peter"", Idade = 15, NumCaracteres = 5, MenorDeIdade = true],
        [Nome = ""Roberta"", Idade = 19, NumCaracteres = 7, MenorDeIdade = false],
        [Nome = ""Paul"", Idade = 18, NumCaracteres = 4, MenorDeIdade = false],
        [Nome = ""Maria"", Idade = 17, NumCaracteres = 5, MenorDeIdade = true]
    })"
            ],
            [
                Description = "Esta função adiciona duas colunas, no qual a primeira conta a quantidade de caracteres da coluna [Nome] e a segunda retorna se a pessoa possui menos de 18 anos, sendo que a primeira é do tipo 'NUMBER' e a segunda do tipo 'LOGICAL'.",
                Code = "fnTableAddColumns(
    Table.FromRecords({
        [Nome = ""Peter"", Idade = 15],
        [Nome = ""Roberta"", Idade = 19],
        [Nome = ""Paul"", Idade = 18],
        [Nome = ""Maria"", Idade = 17]
    }),
    {
        {""NumCaracteres"", each Text.Length([Nome]), Number.Type },
        {""MenorDeIdade"",  each [Idade] < 18, Logical.Type}
    }
)",
                Result = "#table( type table [Nome = any, Idade = any, NumCaracteres = number, MenorDeIdade = logical],
    {
        {""Peter"", 15, 5, true},
        {""Roberta"", 19, 7, false},
        {""Paul"", 18, 4, false},
        {""Maria"", 17, 5, true}
    }
)"
            ]
        }
    ]
in
    Value.ReplaceType(fn, fnType)