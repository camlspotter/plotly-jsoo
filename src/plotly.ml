include Base

module Data = struct
  open Attributes

  type t = Attribute.t list

  let mode   = string "mode"
  let name   = string "name"
  let labels = array "labels" Type.String
  let values = array "values" Type.Float
  let text   = array "text" Type.String
  let orientation = string "orientation"
  let x = array "x" Type.Float
  let y = array "y" Type.Float
  let z = array "z" Type.Float

  let xy xys =
    let xs = Array.map fst xys in
    let ys = Array.map snd xys in
    x xs @ y ys

  let xyz xyzs =
    let xs = Array.map (fun (x,_,_) -> x) xyzs in
    let ys = Array.map (fun (_,y,_) -> y) xyzs in
    let zs = Array.map (fun (_,_,z) -> z) xyzs in
    x xs @ y ys @ z zs

  let data ds = ds

  let to_json = Attributes.to_json
  let of_json = Attributes.of_json
end

module Layout = struct
  open Attributes

  type t = Attribute.t list

  let title = string "title"
  let barmode = string "barmode"

  let layout ats = ats

  let to_json = Attributes.to_json
  let of_json = Attributes.of_json
end

module Graph = struct
  type t = { type_ : string; data : Data.t }

  let scatter data_list = { type_ = "scatter"; data= List.flatten data_list }
  let scatter3d data_list = { type_ = "scatter3d"; data= List.flatten data_list }
  let scattergl data_list = { type_ = "scattergl"; data= List.flatten data_list }
  let bar data_list = { type_ = "bar"; data= List.flatten data_list }
  let pie data_list = { type_ = "pie"; data= List.flatten data_list }
  let histogram data_list = { type_ = "histogram"; data= List.flatten data_list }

  let graph type_ data_list = { type_; data= List.flatten data_list }

  let to_json g =
    match Data.to_json g.data with
    | `O kvs -> `O (("type", `String g.type_) :: kvs)
    | _ -> assert false

  let of_json = function
    | `O kvs ->
        let open Option in
        let type_, others =
          List.partition (function ("type", _) -> true | _ -> false) kvs
        in
        (match type_ with
         | ["type", `String type_] ->
             let+ data = Data.of_json (`O others) in
             { type_; data }
         | _ -> None)
    | _ -> None
end

module Figure = struct
  type t =
    { graphs : Graph.t list;
      layout : Layout.t }

  let figure graphs layout = { graphs; layout= List.flatten layout }

  let to_json g =
    let graphs = List.map Graph.to_json g.graphs in
    let layout = Layout.to_json g.layout in
    (* Not ["graphs"] but ["data"] to make it compatible with
       Plotly JS library *)
    `O ["data", `A graphs; "layout", layout]

  let of_json j =
    let open Option in
    match j with
    | `O kvs ->
        let* graphs = List.assoc_opt "data" kvs in
        let* graphs =
          match graphs with
          | `A graphs -> Option.mapM Graph.of_json  graphs
          | _ -> None
        in
        let* layout = List.assoc_opt "layout" kvs in
        let+ layout = Layout.of_json layout in
        { graphs; layout }
    | _ -> None
end
