(startDate as date, NumberOfWorkingDays as number, optional InclusiveStartDate as nullable logical, optional HolidayDatesList as list) //as date 
=>
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
        Result