module Synthesis

open System.Diagnostics

let abelar xs =
    match xs>12 with
        |true -> match xs<3097 with
                | true -> match xs%12=0 with
                    | true->true
                    |_->false
                 |_->false
         |_->false



      
   

let area bas are =
            match bas<0.0 || are<0.0 with 
            |true-> failwith "Negative area"   
            |_->((are*bas)*0.5)  
          
   

let zollo num =
     match num>0 with
     |true->num*2
     |false->num*(-1)
   

let min num1 num2 =
    match num1>num2 with
    |true->num2
    |_->num1
   

let max first last =
    match first>last with
    |true->first
    |_->last

let ofTime hurs min sec =
    (hurs*60*60)+(min*60)+sec

let toTime sec=
 match sec<0 || sec=0 with
 |true-> (0,0,0)
 |_-> (sec/3600),((sec-(sec/3600)*3600)/60),((sec-(sec/3600)*3600)-(((sec-(sec/3600)*3600))/60)*60)


let digits num =
    let rec Suming value count=
         match value=0 with
        |false->Suming (value/10) (count+1)
        |true-> count
    match num <> 0 with
        |true-> Suming (num) 0
        |_->1

   

let minmax (a,b,c,d )=
 min a b |> min c|>min d,max a b |> max c|>max d 
   

let isLeap year =
 match year>1582 with
  |true->match year%4=0 && year%400=0 with
         |true->true
         |_->false
  |_->failwith "Invalid year"
 
 
   

let month  =function
   
                |1->"January", 31
                |2->"February", 28
                |3->"March", 31
                |4->"April", 30
                |5->"May", 31
                |6->"June", 30
                |7->"July", 31
                |8->"August", 31
                |9->"September", 30
                |10->"October", 31
                |11->"November", 30
                |12->"December", 31
                |_-> failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"