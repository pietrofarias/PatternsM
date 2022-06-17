// Developer: Pietro Farias
// analistax@email.com

let
    fn = (startDate as date, NumberOfWorkingDays as number, optional InclusiveStartDate as nullable logical, optional HolidayDatesList as list) as date  =>
        let
            AddDays = if NumberOfWorkingDays < 0  then -1 else 1,
            HolidayDates = if HolidayDatesList is null then {} else HolidayDatesList,
            InclusiveDay = if InclusiveStartDate is null then false else InclusiveStartDate,
            GenerateList = 
                List.Buffer(
                    List.Generate( () => 
                        [
                            Counter = if InclusiveDay then 1 else 0, 
                            StartDate = startDate,
                            Holiday = List.MatchesAny(HolidayDates, each _ = StartDate),
                            Run = Counter <=  Number.Abs(NumberOfWorkingDays)
                        ],
                        
                        each [Run] = true,
                        each [
                            StartDate = Date.AddDays([StartDate], AddDays),
                            Holiday = List.MatchesAny(HolidayDates, each _ = StartDate),
                            Counter = 
                                if  Date.DayOfWeek(StartDate, Day.Saturday) > 1
                                    and not Holiday 
                                then [Counter] + 1
                                else [Counter], 
                            Run = Counter <=  Number.Abs(NumberOfWorkingDays)
                        ],
                        each [StartDate]
                    )
                ),
            RemoveDateWeekndayAndHoliday = 
                List.Select( 
                    GenerateList, 
                    each 
                        not (
                            let 
                                DateRef = _,
                                IsWeekDay = Date.DayOfWeek(DateRef , Day.Saturday) > 1,
                                IsHoliday = 
                                    if HolidayDates is null 
                                    then false 
                                    else List.MatchesAny(HolidayDates, each _ = DateRef)
                            in IsWeekDay = false or IsHoliday = true
                        )
                ),
            Result = 
                if NumberOfWorkingDays < 0 
                then List.Min(RemoveDateWeekndayAndHoliday) 
                else List.Max(RemoveDateWeekndayAndHoliday)
        in
            Result,
    fnType = type function (
        startDate as date, 
        NumberOfWorkingDays as number, 
        optional InclusiveStartDate as (type logical meta [ Documentation.AllowedValues={true,false}]), 
        optional HolidayDatesList as list
    ) as date meta 
        [
            Documentation.Name = "fnLastOrNextWorkDay",
            Documentation.LongDescription = "Descriptions in pt-BR: Estão função retorna qual o próximo dia útil, ou último, com base em uma data de referência (startDate), considerando uma lista de feriados (opcional) e a possibilidade de se inclusivo com a data de referência (startDate) na contagem de dias úteis. Por padrão não é inclusivo.",
            Documentation.Examples =
            {
                [
                    Description = "Esta função retorna o próximo dia útil, desconsiderando uma os feriados da lista HolidayDatesQuery (lista de datas) e os finais de semana",
                    Code = "LastOrNextWorkDay(#date(2020,12,31), 4, false, HolidayDatesQuery)",
                    Result = "07/01/2021"
                ],
                [
                    Description = "Esta função retorna o próximo dia útil, desconsiderando apenas o final de semana",
                    Code = "LastOrNextWorkDay(#date(2020,12,31), 4, false)",
                    Result = "06/01/2021"
                ],
                [
                    Description = "Esta função retorna o próximo dia útil, desconsiderando apenas o final de semana e incluindo a data de referência na contagem",
                    Code = "LastOrNextWorkDay(#date(2020,12,31), 4, false)",
                    Result = "05/01/2021"
                ],
                [
                    Description = "Esta função retorna o próximo dia útil, desconsiderando apenas o final de semana, incluindo a data de referência na contagem, mas a data de referência é um sábado",
                    Code = "LastOrNextWorkDay(#date(2021,1,2), 4, true)",
                    Result = "07/01/2021"
                ],
                [
                    Description = "Esta função retorna o útil dia útil, desconsiderando uma os feriados da lista HolidayDatesQuery (lista de datas) e os finais de semana",
                    Code = "LastOrNextWorkDay(#date(2020,1,4), -1, false, HolidayDatesQuery)",
                    Result = "30/12/2020"
                ]
            }

        ]
in
    Value.ReplaceType(fn, fnType)