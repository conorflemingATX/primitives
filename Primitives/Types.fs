module Primitives.Types

open System.Text.RegularExpressions
open FSharpPlus
open FSharpPlus.Data

type ValidationErrors = string list
type Val<'value> = Validation<ValidationErrors, 'value>

type IPrimitive<'ofT, 'toT, 'brand> =
    abstract member value: 'toT
    abstract member make: 'ofT -> Val<IPrimitive<'ofT, 'toT, 'brand>>

module Utils =
    let value (x: IPrimitive<'ofT, 'toT, 'brand>) : 'toT = x.value

    let validationFromPredicate (message: string) (pred: 'a -> bool) : 'a -> Val<'a> =
        (fun x ->
            if pred x then
                Success x
            else
                Failure [ message ])

    let composeValidations (v: 'a -> Val<'b>) (v': 'b -> Val<'c>) = v >> Validation.bind v'

    let consFromValidation (cons: 'a -> 'b) (validation: 'a -> Val<'a>) : 'a -> Val<'b> =
        validation >> Validation.map cons

    let createMake<'ofT, 'brand, 'toT when 'brand :> IPrimitive<'ofT, 'toT, 'brand>>
        (message: string)
        (pred: 'ofT -> bool)
        (cons: 'ofT -> 'brand)
        : 'ofT -> Val<IPrimitive<'ofT, 'toT, 'brand>> =
        consFromValidation (fun x -> cons x) (validationFromPredicate message pred)

    let composeMake<'ofT, 'toT, 'toT2, 'brand, 'brand2 when 'brand :> IPrimitive<'ofT, 'toT, 'brand> and 'brand2 :> IPrimitive<'ofT, 'toT, 'brand2>>
        (make: 'ofT -> Val<IPrimitive<'ofT, 'toT, 'brand>>)
        (make': 'toT -> Val<IPrimitive<'toT, 'toT2, 'brand2>>)
        =
        make >> Validation.bind (value >> make')


[<AutoOpen>]
module NotNullString =
    [<Literal>]
    let private NULL_VALUE_MESSAGE = @"Value cannot be null."

    let private isNotNull x = x <> null

    type NotNullString private (v: string) =
        interface IPrimitive<string, string, NotNullString> with
            member _.value = v

            member _.make(s: string) =
                Utils.createMake NULL_VALUE_MESSAGE isNotNull NotNullString s

        static member make(s: string) : Val<IPrimitive<string, string, NotNullString>> =
            (NotNullString s :> IPrimitive<string, string, NotNullString>)
                .make s

[<AutoOpen>]
module StringOfPattern =
    [<Literal>]
    let private BAD_FORMATTING_MESSAGE = @"String does not match the correct format."

    let private isMatch (pattern: string) = (flip tuple2) pattern >> Regex.IsMatch

    type StringOfPattern private (pattern: string, v: string) =
        interface IPrimitive<string, string, StringOfPattern> with
            member _.value = v

            member _.make(s: string) =
                Utils.createMake BAD_FORMATTING_MESSAGE (isMatch pattern) (tuple2 pattern >> StringOfPattern) s

        static member make (pattern: string) (s: string) =
            (StringOfPattern(pattern, s) :> IPrimitive<string, string, StringOfPattern>)
                .make s

[<AutoOpen>]
module Email =
    type Email private (v: string) =
        interface IPrimitive<string, string, Email> with
            member _.value = v

            member _.make(s: string) =
                s
                |> StringOfPattern.make ""
                |> Validation.map (Utils.value)
                |> Validation.map (fun x -> Email x :> IPrimitive<string, string, Email>)
