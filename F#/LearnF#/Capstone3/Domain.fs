namespace Capstone3.Domain

open System


type Customer = { Name : string }

type Account = { AccountId : Guid; Owner : Customer; Balance : decimal }

type Transaction = { Timestamp : System.DateTime; Operation : string; Amount : decimal; Accepted : bool }
