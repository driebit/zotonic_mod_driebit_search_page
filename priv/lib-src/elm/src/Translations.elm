module Translations exposing (..)

import Json.Decode as Decode exposing (Decoder)


type alias Translation =
    { en : String
    , nl : String
    , de : String
    }


type Language
    = EN
    | NL
    | DE


languageFromJson : Decoder Language
languageFromJson =
    Decode.string
        |> Decode.andThen
            (\lang ->
                case lang of
                    "en" ->
                        Decode.succeed EN

                    "nl" ->
                        Decode.succeed NL

                    "de" ->
                        Decode.succeed DE

                    _ ->
                        Decode.fail ("Unknown language: " ++ lang)
            )


translate : Language -> Translation -> String
translate lang translation =
    case lang of
        EN ->
            translation.en

        NL ->
            translation.nl

        DE ->
            translation.de


translations =
    { searchPlaceholder = { en = "Search", nl = "Zoeken", de = "Suchen" }
    , noSearchYet = { en = "No search has been performed yet.", nl = "Er is nog geen zoekopdracht uitgevoerd.", de = "Es wurde noch keine Suche durchgeführt." }
    , loading = { en = "Loading...", nl = "Laden...", de = "Wird geladen..." }
    , waitingForConnection = { en = "Waiting for connection...", nl = "Wachten op verbinding...", de = "Warten auf Verbindung..." }
    , noResultsFound = { en = "No results found.", nl = "Geen resultaten gevonden.", de = "Keine Ergebnisse gefunden." }
    , results = { en = "results", nl = "resultaten", de = "Ergebnisse" }
    , errorPrefix = { en = "Error: ", nl = "Fout: ", de = "Fehler: " }
    , previous = { en = "Previous", nl = "Vorige", de = "Vorherige" }
    , next = { en = "Next", nl = "Volgende", de = "Nächste" }
    , sortTitle = { en = "Title", nl = "Titel", de = "Titel" }
    , sortModified = { en = "Modified", nl = "Gewijzigd", de = "Geändert" }
    , sortCreated = { en = "Created", nl = "Aangemaakt", de = "Erstellt" }
    , fixedRangesCustom = { en = "Custom", nl = "Aangepast", de = "Benutzerdefiniert" }
    , fixedRangesFrom = { en = "From", nl = "Van", de = "Von" }
    , fixedRangesTo = { en = "To", nl = "Tot", de = "Bis" }
    , fixedRangesPlaceholder = { en = "dd-mm-yyyy", nl = "dd-mm-jjjj", de = "TT-MM-JJJJ" }
    , fixedRangesLast7Days = { en = "Last 7 Days", nl = "Laatste 7 dagen", de = "Letzte 7 Tage" }
    , fixedRangesLastMonth = { en = "Last Month", nl = "Laatste maand", de = "Letzter Monat" }
    , fixedRangesLastYear = { en = "Last Year", nl = "Laatste jaar", de = "Letztes Jahr" }
    , fixedRangesPreviousYear = { en = "Previous Year", nl = "Vorig jaar", de = "Vorheriges Jahr" }
    , fixedRangesNextWeek = { en = "Next Week", nl = "Volgende week", de = "Nächste Woche" }
    , fixedRangesNextMonth = { en = "Next Month", nl = "Volgende maand", de = "Nächster Monat" }
    , fixedRangesUpcoming = { en = "Upcoming", nl = "Komend", de = "Bevorstehend" }
    , fixedRangesToday = { en = "Today", nl = "Vandaag", de = "Heute" }
    , fixedRangesCustomRange = { en = "Custom Range", nl = "Aangepast bereik", de = "Benutzerdefinierter Bereich" }
    , dropdownAll = { en = "All", nl = "Alle", de = "Alle" }
    , multiselectSearchPlaceholder = { en = "Search options...", nl = "Opties zoeken...", de = "Optionen suchen..." }
    , multiselectShowMore = { en = "+ Show More", nl = "+ Meer tonen", de = "+ Mehr anzeigen" }
    , searchFilters = { en = "Search Filters", nl = "Zoekfilters", de = "Suchfilter" }
    }
