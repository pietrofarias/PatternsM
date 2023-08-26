// Developer: Pietro Farias
// contato@analistax.com
let
    fn =
        (
            DataRef as date,
            MinDate as date,
            optional EndOfCalendar as number, //0=EndOfYear; 1=EndOfMonth
            optional Culture as text, //Default="pt-br"
            optional FirstDayWeek as number //Default Sunday
        ) =>
        let
            FirstDayOfWeek = if FirstDayWeek = null then Day.Sunday else FirstDayWeek,
            defineCulture = if Culture = null then "pt-br" else Culture,
            fnTableAddColumns = ( EditedTable as table, ListColumns as list) as table =>
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
            ListWithDates = ()=>
            if DataRef < MinDate
            then error "A 'DataRef' não pode ser menor do que a 'MinDate"
            else
                List.Generate(
                    () =>
                        [
                            PrimeiraData = Date.StartOfYear(MinDate),
                            UltimaData = if EndOfCalendar = 1 then Date.EndOfMonth(DataRef) else Date.EndOfYear(DataRef),
                            DataContexto = PrimeiraData
                        ],
                    each [DataContexto] <= [UltimaData],
                    each
                        [
                            PrimeiraData = [PrimeiraData],
                            UltimaData = [UltimaData],
                            DataContexto = [DataContexto] + #duration(1, 0, 0, 0)
                        ],
                    each [DataContexto]
                ),
            ConvertListToTable = Table.FromList(
                ListWithDates(), Splitter.SplitByNothing(), {"Data"}, {"Data"}, ExtraValues.Ignore
            ),
            ChangedDateType = Table.TransformColumnTypes(ConvertListToTable, {{"Data", type date}}),
            BasicDatesColumns = fnTableAddColumns(
                ChangedDateType,
                {
                    {"Ano", each Date.Year([Data]), Int16.Type},
                    {"Mês", each Date.Month([Data]), Int8.Type},
                    {"Mês Nome", each Date.MonthName([Data], defineCulture), Text.Type},
                    {"Dias do Mês", each Date.DaysInMonth([Data]), Int8.Type},
                    {"Trimestre", each Date.QuarterOfYear([Data]), Int8.Type},
                    {"Ano Trimestre", 
                        each Text.From(Date.Year([Data])) 
                            & "/" 
                            & Number.ToText( Date.QuarterOfYear([Data]), "00" ),
                        Text.Type
                    },
                    {"Semana do Ano", each Date.WeekOfYear([Data]), Int8.Type},
                    {"Semana do Mês", each Date.WeekOfMonth([Data]), Int8.Type},
                    {"Início da Semana", each Date.StartOfWeek([Data], FirstDayOfWeek), Date.Type},
                    {"Término da Semana", each Date.EndOfWeek([Data], FirstDayOfWeek), Date.Type},
                    {"Dia", each Date.Day([Data]), Int8.Type},
                    {"Dia da Semana", each Date.DayOfWeek([Data], FirstDayOfWeek), Int8.Type},
                    {"Nome do Dia da Semana", each Date.DayOfWeekName([Data], defineCulture), Text.Type},
                    {"Dia do Ano", each Date.DayOfYear([Data]), Int16.Type}
                }
            )
        in
            BasicDatesColumns,
    fnType = type function (
        DataRef as (
            type date meta [
                Documentation.FieldCaption = "Última data de referência",
                Documentation.FieldDescription = "Usa esta data como a data de referência máxima. A tabela calendário irá retornar datas até o mês ou ano desta data.",
                Documentation.SampleValues = {#date(2023,6,12)}
            ]
        ),
        MinDate as (
            type date meta [
                Documentation.FieldCaption = "Menor data de referência",
                Documentation.FieldDescription = "Usa esta data como referência para retornar o primeiro dia do ano, como data inicial na tabela calendário",
                Documentation.SampleValues = {#date(2022,2,15)}
            ]
        ),
        optional EndOfCalendar as (
            type number meta [
                Documentation.AllowedValues = {0,1},
                Documentation.FieldCaption = "Último dia da tabela",
                Documentation.FieldDescription = "O valor 0, valor padrão, irá retornar até o último dia do ano da DataRef. O Valor 1, irá retornar até o último dia do mês da DataRef"
            ]
        ), 
        optional Culture as (
            type text meta [
                Documentation.SampleValues = {"pt-br"}
            ]
        ), 
        optional FirstDayWeek as (
            type number meta [
                Documentation.AllowedValues = {
                    Day.Sunday,
                    Day.Monday,
                    Day.Tuesday,
                    Day.Wednesday,
                    Day.Thursday,
                    Day.Friday,
                    Day.Saturday
                }
            ]
        )
    ) as table
in
    Value.ReplaceType(fn, fnType)