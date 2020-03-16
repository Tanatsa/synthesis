module Synthesis

let abelar a = a>12 && a<3097 && a%12=0
failwith "Not implemented"


let area b h =  
    match b<0.0 || h<0.0 with
    |false->(0.5*b)*h
    |_-> failwith "negative values"
failwith "Not implemented"


let zollo x =
    match x<0 with
    |true-> -1*x
    |_-> x*2
failwith "Not implemented"

let min a b = 
    match a<b with
    |true -> a
    |_ -> b
failwith "Not implemented"

let max a b = 
    match a<b with
    |true -> b
    |_ -> a
failwith "Not implemented"

let ofTime hr min sec = (hr*3600) + (min*60) + sec
failwith "Not implemented"

let toTime sec =
    match sec<0 with
    |true->(0,0,0)
    |_-> ((sec/3600), ((sec%3600)/60), ((sec%3600)%60))
failwith "Not implemented"

let rec digits x =
    match x < 0 with
    |false-> 
        match (x = 0)||(x = 1)||(x = 2)||(x = 3)||(x = 4)||(x = 5)||(x = 6)||(x = 7)||(x = 8)||(x = 9) with
        |true-> 1
        |_-> 1+ digits (x/10)
    |_-> 
        match (x = -1)||(x = -2)||(x = -3)||(x = -4)||(x = -5)||(x = -6)||(x = -7)||(x = -8)||(x = -9) with
        |true -> 1
        |_-> 1 + digits ((x/10)* -1)
failwith "Not implemented"

let minmax (a,b,c,d) =
     let smaller_value = min d c |> min b |> min a
     let larger_value = max c b |> max a |> max d
     (smaller_value,larger_value)
failwith "Not implemented"

let isLeap  yr =
    match yr < 1582 with
    |true->failwith "too far behind"
    |_-> 
        match ((yr%4=0)&&(not(yr%100=0)))  || (yr%400=0) with
        |true-> true
        |_->false
failwith "Not implemented"

let month x =
    match x < 1 || x > 12 with
    |true->failwith "there is only 12 months in a year, number of month supplied is invalid"
    |_-> 
        match x with
        |1->("January",31)
        |2->("February",28)
        |3->("March",31)
        |4->("April",30)
        |5->("May",31)
        |6->("June",30)
        |7->("July",31)
        |8->("August",31)
        |9->("September",30)
        |10->("October",31)
        |11->("November",30)
        |12->("December",31)
            
failwith "Not implemented"

let rec toBinary y=
    match y<0 with
    |true->failwith "umm negative numders are invalid"
    |_->
        match y/2 = 0 with
        |true-> 
            match (y%2) with
            |0 -> "0"
            |_-> "1"
        |_->toBinary (y/2) + 
            match (y%2) with
            |0 -> "0"
            |_-> "1"               
failwith "Not implemented"

let bizFuzz n =
    let rec bizfuzz x (a,b,c)=
        match x>=1 && x<=n with
        |true-> 
            match (x%3=0,x%5=0,(x%3=0 && x%5=0)) with
            |_,_,true->bizfuzz (x+1) (a+1,b+1,c+1)
            |true,false,_-> bizfuzz (x+1) (a+1,b,c)
            |false,true,_-> bizfuzz (x+1) (a,b+1,c)
            |_->bizfuzz (x+1) (a,b,c)

        |false-> a,b,c
        |_->failwith "number out of range"
    bizfuzz 1 (0,0,0)    
failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"