// Developer: Pietro Farias
// analistax@email.com
let
    fn = (
        startDate as date,
        EndDate as date,
        optional InclusiveStartDate as nullable logical,
        optional HolidayDatesList as list,
        optional ConsiderWeekendsAsWorkingDays as number
    ) as number =>
        let
            FirstDayOfWeekOptional = (dateSelect as date) =>
                if ConsiderWeekendsAsWorkingDays = null or ConsiderWeekendsAsWorkingDays = 0 then
                    Date.DayOfWeek(dateSelect, Day.Saturday) <= 1
                else if ConsiderWeekendsAsWorkingDays = 1 then
                    false
                else if ConsiderWeekendsAsWorkingDays = 2 then
                    Date.DayOfWeek(dateSelect, Day.Sunday) = 0
                else if ConsiderWeekendsAsWorkingDays = 3 then
                    Date.DayOfWeek(dateSelect, Day.Saturday) = 0
                else
                    error "O parâmetro 'ConsiderWeekendsAsWorkingDays' é inválido. O valor só pode ser nulo ou de 0..3",
            HolidayDates = if HolidayDatesList is null then {} else HolidayDatesList,
            InclusiveDay = if InclusiveStartDate is null then false else InclusiveStartDate,
            DurationDays = Duration.Days(EndDate - startDate),
            GenerateList = List.Buffer(
                List.Dates(
                    if InclusiveDay then
                        startDate
                    else
                        Date.AddDays(startDate, 1),
                    DurationDays + Number.From(InclusiveDay),
                    #duration(1, 0, 0, 0)
                )
            ),
            WeekendDates = List.Select(GenerateList, each FirstDayOfWeekOptional(_)),
            RemoveDateWeekndayAndHoliday = List.RemoveMatchingItems(GenerateList, HolidayDates & WeekendDates),
            Result = List.Count(RemoveDateWeekndayAndHoliday)
        in
            Result,
    fnType = type function (
        startDate as date,
        EndDate as date,
        optional InclusiveStartDate as (type logical meta [Documentation.AllowedValues = {true, false}]),
        optional HolidayDatesList as list,
        optional ConsiderWeekendsAsWorkingDays as (
            type number meta [
                Documentation.AllowedValues = {null, 0, 1, 2, 3},
                Documentation.FieldCaption = "Considerar final de semana como dia últi?",
                Documentation.FieldDescription = "0/null = Não considerar final de semana como dia útil, 1-considerar final de semana como dia útil, 2-Considerar sábado dia útil, 3-Considerar domingo dia útil"
            ]
        )
    ) as date meta [
        Documentation.Name = "fnNumberWorkDay",
        Documentation.LongDescription = "Descriptions in pt-BR: Esta função retorna a quantidade de dias úteis com base em uma data de referência (startDate), considerando uma lista de feriados (opcional) e a possibilidade de se inclusivo com a data de referência (startDate) na contagem. Por padrão não é inclusivo. O parâmetro 'ConsiderWeekendsAsWorkingDays', é usado para considerar o final de semana como dia útil. Os valores possiveis para o parâmetro 'ConsiderWeekendsAsWorkingDays' são: 0/null = Não considerar final de semana como dia útil, 1-considerar final de semana como dia útil, 2-Considerar sábado dia útil, 3-Considerar domingo dia útil",
        Documentation.Examples = {
            [
                Description = "Esta função retorna a quantidade de dias úteis, desconsiderando uma os feriados da lista HolidayDatesQuery (lista de datas) e os finais de semana",
                Code = "NumberWorkDay(#date(2020,1,7), #date(2020,12,31), false, HolidayDatesQuery)",
                Result = "4"
            ],
            [
                Description = "Esta função retorna a quantidade de dias úteis, desconsiderando apenas o final de semana",
                Code = "LastOrNextWorkDay(#date(2020,1,6), #date(2020,12,31), false)",
                Result = "4"
            ],
            [
                Description = "Esta função retorna a quantidade de dias úteis, desconsiderando apenas o final de semana e incluindo a data de referência na contagem",
                Code = "LastOrNextWorkDay(#date(2020,1,5), #date(2020,12,31), false)",
                Result = "4"
            ],
            [
                Description = "Esta função retorna a quantidade de dias úteis, desconsiderando apenas o final de semana, incluindo a data de referência na contagem, mas a data de referência é um sábado",
                Code = "LastOrNextWorkDay(#date(2020,1,7), #date(2021,1,2), true)",
                Result = "4"
            ],
            [
                Description = "Esta função retorna a quantidade de dias úteis, considerando o sábado como dia útil, incluindo a data de referência na contagem",
                Code = "LastOrNextWorkDay(#date(2024,4,25), #date(2024,4,30)",
                Result = "5"
            ]
        }
    ]
in
    Value.ReplaceType(fn, fnType)