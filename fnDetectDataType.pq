fnDetectDataType = (Table as table, optional ColumnsName as list) as table =>
    let
        listColumnsName = 
            if ColumnsName = null 
            then Table.ColumnNames( Table ) 
            else ColumnsName,

        ColumnsNumber = List.Count(listColumnsName),

        listGen =
            List.Generate(
                ()=> [
                    vCounter = 0,
                    iValue = Table.Column(Table,listColumnsName{vCounter}){0},
                    tpValue = Value.Type( Value.FromText(iValue) ),
                    returnType = 
                        if tpValue <> Text.Type
                            and tpValue <> Time.Type
                            and tpValue <> Duration.Type
                            and tpValue <> Date.Type
                            and tpValue <> DateTime.Type
                            and tpValue <> Number.Type
                            and tpValue <> Percentage.Type
                            and tpValue <> DateTimeZone.Type
                            and tpValue <> Int64.Type
                        then Text.Type
                        else tpValue,
                    vReturn = {listColumnsName{vCounter}, returnType}
                ],
                each  [vCounter] < ColumnsNumber,
                each [
                    vCounter = [vCounter] + 1,
                    iValue = Table.Column(Table,listColumnsName{vCounter}){0},
                    tpValue = Value.Type( Value.FromText(iValue) ),
                    returnType = 
                        if tpValue <> Text.Type
                            and tpValue <> Time.Type
                            and tpValue <> Duration.Type
                            and tpValue <> Date.Type
                            and tpValue <> DateTime.Type
                            and tpValue <> Number.Type
                            and tpValue <> Percentage.Type
                            and tpValue <> DateTimeZone.Type
                            and tpValue <> Int64.Type
                        then Text.Type
                        else tpValue,
                    vReturn = {listColumnsName{vCounter}, returnType}
                ],
                each [vReturn] 
            ),
        
        transformColumns =
            Table.TransformColumnTypes(
                Table,
                listGen
            )
    in
        transformColumns
