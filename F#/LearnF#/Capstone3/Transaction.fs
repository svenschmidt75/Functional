module Capstone3.Transaction

open System
open Capstone3.Domain


let deserialize (fileContents : string) =
        let parts = fileContents.Split([|"***"|], StringSplitOptions.None)
        let timePart = parts.[0].Split([|" "|], StringSplitOptions.None)
        let timeStampPart = sprintf "%s %s" timePart.[2] timePart.[3]
        { Timestamp = DateTime.Parse timeStampPart
          Operation = parts.[1]
          Amount = Decimal.Parse parts.[2]
          Accepted = Boolean.Parse parts.[3] }
