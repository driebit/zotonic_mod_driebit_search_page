module Sort exposing (..)


type SortBy
    = Id Order
    | Title Order
    | DateCreated Order
    | DateModified Order
    | Custom String Order


type Order
    = Ascending
    | Descending


type Msg
    = SortByChanged SortBy


update : Msg -> SortBy -> SortBy
update msg sortBy =
    case msg of
        SortByChanged newSortBy ->
            newSortBy


sortByToTitle : SortBy -> String
sortByToTitle sortBy =
    case sortBy of
        Id Ascending ->
            "ID (ascending)"

        Id Descending ->
            "ID (descending)"

        Title Ascending ->
            "Title (ascending)"

        Title Descending ->
            "Title (descending)"

        DateCreated Ascending ->
            "Date created (ascending)"

        DateCreated Descending ->
            "Date created (descending)"

        DateModified Ascending ->
            "Date modified (ascending)"

        DateModified Descending ->
            "Date modified (descending)"

        Custom field Ascending ->
            field ++ " (ascending)"

        Custom field Descending ->
            field ++ " (descending)"


sortByToString : SortBy -> String
sortByToString sortBy =
    case sortBy of
        Id Ascending ->
            "+id"

        Id Descending ->
            "-id"

        Title Ascending ->
            "+title"

        Title Descending ->
            "-title"

        DateCreated Ascending ->
            "+date_created"

        DateCreated Descending ->
            "-date_created"

        DateModified Ascending ->
            "+date_modified"

        DateModified Descending ->
            "-date_modified"

        Custom field Ascending ->
            "+" ++ field

        Custom field Descending ->
            "-" ++ field
