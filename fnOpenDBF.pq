(sourceFolder as text, optional fileDbfName as text) as table =>
    let
        FileProvided = fileDbfName <> null,
        Source = Folder.Contents(sourceFolder),
        DbfFiles = Table.SelectRows(
            Source,
            each
                Text.Upper([Extension]) = ".DBF"
                and ([Attributes][Hidden] = false and[Attributes][System] = false)
                and (if FileProvided then [Name] = fileDbfName else true)
        ),
        ConnectOleDb = Table.AddColumn(
            DbfFiles,
            "Table_DBF",
            each
                OleDb.DataSource(
                    "provider=Microsoft.ACE.OLEDB.12.0;data source=" & [Folder Path] & ";extended properties=dBASE IV",
                    [
                        Query = "select * from " & [Name]
                    ]
                ),
            Table.Type
        ),
        SelectComuns = Table.SelectColumns(ConnectOleDb, {"Name", "Table_DBF"}),
        ReturnFunction = if FileProvided then SelectComuns{0}[Table_DBF] else SelectComuns
    in
        ReturnFunction
