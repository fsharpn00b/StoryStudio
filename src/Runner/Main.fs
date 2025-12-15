module Main

open Browser.Dom
open Feliz

open Runner_UI

let root = ReactDOM.createRoot <| document.getElementById "root"
root.render <| Runner_Test ()
