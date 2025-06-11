// =============================================================================
// Function: fnBuildProcasQuery
// Description: Dynamically constructs an OData query string for the Procas API,
//              supporting $filter, $select, and $top options.
// Author: P. Farias
// Repository: https://github.com/pietrofarias/PatternsM | Version: 2.0
// =============================================================================
//
// Example parameters for testing:
/*
    tableName = "GLDetail",
    companyCode = "SAMPLE",
    filterSchema = {
        [
            field = "Account",
            operator = "eq",
            values = {"6110000", "6150000"}
        ],
        [
            field = "LCItem",
            operator = "eq",
            values = {"E"}
        ]
    },
    selectSchema = {
        "GLTransLine",
        "Account",
        "GLAmount"
    },
    options = [
        filter = filterSchema,
        select = selectSchema,
        top = 5
    ]
*/
let
    fn = (companyCode as text, tableName as text, optional options as nullable record) =>
        let
            hasFilter    = Record.HasFields(options, "filter"),
            hasSelect    = Record.HasFields(options, "select"),
            hasTop       = Record.HasFields(options, "top"),
            hasAnyQuery  = try hasFilter or hasSelect or hasTop otherwise false,

            strFilter =
                if hasFilter then
                    "$filter=" &
                    Text.Combine(
                        List.Transform(
                            options[filter],
                            each Text.Combine(
                                List.Transform(
                                    _[values],
                                    (v) =>
                                        let
                                            isDateTime = try DateTime.From(v) otherwise null,
                                            formatted = 
                                                if isDateTime <> null then
                                                    DateTime.ToText(isDateTime, "yyyy-MM-ddThh:mm:ssZ")
                                                else if Value.Is(v, type number) then
                                                    Text.From(v)
                                                else
                                                    "'" & Text.From(v) & "'"
                                        in
                                            _[field] & " " & _[operator] & " " & formatted
                                ),
                                " or "
                            )
                        ),
                        " and "
                    )
                else
                    "",

            strTop =
                if hasTop then
                    "$top=" & Text.From(options[top])
                else
                    "",

            strSelect =
                if hasSelect then
                    "$select=" & Text.Combine(options[select], ",")
                else
                    "",

            strQuery =
                if hasAnyQuery then
                    "?" &
                    Text.Combine(
                        List.Select({strFilter, strSelect, strTop}, each _ <> ""),
                        "&"
                    )
                else
                    "",

            baseUrl = "https://data.procas.com/ProcasData/v1/" & companyCode & "/",
            fullUrl = baseUrl & tableName & strQuery,

            result = OData.Feed(fullUrl, null, [Implementation = "2.0"])
        in
            result,

    fnType = type function (
        companyCode as text,
        tableName as text, 
        optional options as nullable record
    ) as table
        meta [
            Documentation.Name = "fnBuildProcasQuery",
            Documentation.Description = "Builds a Procas OData query with optional filter, select, and top clauses.",
            Documentation.LongDescription = 
                "This function dynamically generates a Procas OData query using the given company code, table name and options. It supports multi-field filtering using logical AND/OR, column projection via $select, and row limiting via $top.",
            Documentation.Examples = {
                [
                    Description = "Get top 5 records from GLDetail where Account is 6110000 or 6150000",
                    Code = "fnBuildProcasQuery(""SAMPLE"", ""GLDetail"", [filter = { [field = ""Account"", operator = ""eq"", values = {""6110000"", ""6150000""}] }, select = {""GLTransLine"", ""Account"", ""GLAmount""}, top = 5])",
                    Result = "Returns a table with the specified filtered data"
                ]
            },
            Documentation.Author = "P. Farias"
        ]
in
    Value.ReplaceType(fn, fnType)
