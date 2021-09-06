module RateLimiter exposing (Msg, RateLimiter, days, hours, minutes, seconds, slidingLog, sub, trigger, update, weeks)

{-| This library provides a simple sliding log rate limiter. It should be fed a Posix.Posix using a Time.every subscription.


# Constructor

@docs slidingWindow

-}

import Dict exposing (Dict)
import Time exposing (Posix)
import TypedTime exposing (TypedTime)


type alias Bucket =
    List Posix


type RateLimiter comparable
    = RateLimiter
        { now : Maybe Posix
        , size : Int
        , windowInSeconds : Int
        , buckets : Dict comparable Bucket
        }


type Msg
    = Tick Posix


weeks : Float -> TypedTime
weeks count =
    days count |> TypedTime.multiply 7


days : Float -> TypedTime
days count =
    TypedTime.hours 24 |> TypedTime.multiply count


hours : Float -> TypedTime
hours =
    TypedTime.hours


minutes : Float -> TypedTime
minutes =
    TypedTime.minutes


seconds : Float -> TypedTime
seconds =
    TypedTime.seconds


sub : RateLimiter comparable -> Sub Msg
sub (RateLimiter { windowInSeconds }) =
    Time.every (toFloat windowInSeconds * 10) Tick


{-| Create a sliding log rate limiter that allows 5 operations every 5 minutes.

    slidingLog 5 (minutes 5)

-}
slidingLog : Int -> TypedTime -> RateLimiter comparable
slidingLog size window =
    RateLimiter { now = Nothing, size = size, windowInSeconds = window |> TypedTime.toSeconds |> round, buckets = Dict.empty }


{-| Update the current time.
-}
update : Msg -> RateLimiter comparable -> RateLimiter comparable
update (Tick now) (RateLimiter model) =
    RateLimiter { model | now = Just now }


{-| Try to execute an operation. If allowed, the accept function is called with an updated RateLimiter instance and then returned. If not allowed, the reject argument is returned.

    let
        accept =
            \rl -> ( { model | rateLimiter = rl }, expensiveHttpRequest RequestCompleted } )
    in
    RateLimiter.trigger rateLimiter identifier accept ( model, Cmd.none )

-}
trigger : RateLimiter comparable -> comparable -> (RateLimiter comparable -> response) -> response -> response
trigger (RateLimiter ({ now, size, windowInSeconds, buckets } as state)) comp accept reject =
    case now of
        Nothing ->
            reject

        Just now_ ->
            let
                cutoffTime =
                    Time.posixToMillis now_ - (windowInSeconds * 1000)

                bucket =
                    Dict.get comp buckets
                        |> Maybe.withDefault []
                        |> (::) now_
                        |> List.filter
                            (\timestamp -> Time.posixToMillis timestamp > cutoffTime)
            in
            if List.length bucket <= size then
                accept <| RateLimiter { state | buckets = Dict.insert comp bucket buckets }

            else
                reject
