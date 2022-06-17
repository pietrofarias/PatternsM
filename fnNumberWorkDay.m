// Developer: Pietro Farias
// analistax@email.com
let
    fn = (startDate as date, EndDate as date, optional InclusiveStartDate as nullable logical, optional HolidayDatesList as list) /*as number*/  =>
        let
            HolidayDates = if HolidayDatesList is null then {} else HolidayDatesList,
            InclusiveDay = if InclusiveStartDate is null then false else InclusiveStartDate,
            DurationDays = Duration.Days(EndDate - startDate),
            GenerateList = 
                List.Buffer(
                    List.Generate( () => 
                        [
                            Counter = 0,
                            StartDate = startDate,
                            Holiday = List.MatchesAny(HolidayDates, each _ = StartDate),
                            Run = Counter <=  Number.Abs(DurationDays)
                        ],
                        
                        each [Run] = true and [StartDate] <= EndDate,
                        each [
                            StartDate = Date.AddDays([StartDate], 1),
                            Holiday = List.MatchesAny(HolidayDates, each _ = StartDate),
                            Counter = 
                                if  Date.DayOfWeek(StartDate, Day.Saturday) > 1
                                    and not Holiday 
                                then [Counter] + 1
                                else [Counter], 
                            Run = Counter <=  Number.Abs(DurationDays)
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
                        ) and (
                            if InclusiveDay then _ >= startDate else _ > startDate
                        )
                ),
            Result = List.Count(RemoveDateWeekndayAndHoliday)
        in
            Result,
    fnType = type function (
        startDate as date, 
        EndDate as date, 
        optional InclusiveStartDate as (type logical meta [ Documentation.AllowedValues={true,false}]), 
        optional HolidayDatesList as list
    ) as date meta 
        [
            Documentation.Name = "fnNumberWorkDay",
            Documentation.LongDescription = "Descriptions in pt-BR: Estão função retorna a quantidade de dias úteis com base em uma data de referência (startDate), considerando uma lista de feriados (opcional) e a possibilidade de se inclusivo com a data de referência (startDate) na contagem. Por padrão não é inclusivo.",
            Documentation.Examples =
            {
                [
                    Description = "Esta função retorna a quantidade de dias úteis, desconsiderando uma os feriados da lista HolidayDatesQuery (lista de datas) e os finais de semana",
                    Code = "NumberWorkDay(#date(2020,12,31), #date(2020,1,7), false, HolidayDatesQuery)",
                    Result = "4"
                ],
                [
                    Description = "Esta função retorna a quantidade de dias úteis, desconsiderando apenas o final de semana",
                    Code = "LastOrNextWorkDay(#date(2020,12,31), #date(2020,1,6), false)",
                    Result = "4"
                ],
                [
                    Description = "Esta função retorna a quantidade de dias úteis, desconsiderando apenas o final de semana e incluindo a data de referência na contagem",
                    Code = "LastOrNextWorkDay(#date(2020,12,31), #date(2020,1,5), false)",
                    Result = "4"
                ],
                [
                    Description = "Esta função retorna a quantidade de dias úteis, desconsiderando apenas o final de semana, incluindo a data de referência na contagem, mas a data de referência é um sábado",
                    Code = "LastOrNextWorkDay(#date(2021,1,2), #date(2020,1,7), true)",
                    Result = "4"
                ]
            }

        ]
in
    Value.ReplaceType(fn, fnType)