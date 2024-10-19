open Plotly

let rec of_value v =
  let open Value in
  match v with
  | Value (Float, f) -> Py.Float.of_float f
  | Value (String, s) -> Py.String.of_string s
  | Value (Array ty, vs) ->
      Py.List.of_array @@ Array.map (fun v -> of_value (Value (ty, v))) vs

let of_attribute (s, v : Attribute.t) = (s, of_value v)

let of_attributes = List.map of_attribute

let init () =
  if not @@ Py.is_initialized () then Py.initialize ()

let go =
  init ();
  Py.Import.import_module "plotly.graph_objects"

type _ t = Py.Object.t

type figure

let of_graph Graph.{type_; data} =
  let c =
    match type_ with
    | "scatter" -> "Scatter"
    | "scatter3d" -> "Scatter3d"
    | "scattergl" -> "Scattergl"
    | "bar" -> "Bar"
    | "pie" -> "Pie"
    | _ -> assert false
  in
  Py.Module.get_function_with_keywords go c [||]
    (of_attributes (data :> Attribute.t list))

let of_layout layout =
  let layout = (layout : Layout.t :> Attribute.t list) in
  Py.Dict.of_bindings_map
    Py.String.of_string
    of_value
    layout

let of_figure fig : figure t =
  let data = Py.List.of_list_map of_graph fig.Figure.graphs in
  let layout = of_layout fig.layout in
  Py.Module.get_function_with_keywords go "Figure" [||]
    ["data", data;
     "layout", layout]

let show ?renderer figure =
  match Py.Object.get_attr_string figure "show" with
  | None -> failwith "no show"
  | Some show ->
      let show = Py.Callable.to_function_with_keywords show [||] in
      match renderer with
      | None -> ignore @@ show []
      | Some n -> ignore @@ show ["renderer", Py.String.of_string n]

let write_image figure path =
  match Py.Object.get_attr_string figure "write_image" with
  | None -> failwith "write_image"
  | Some f ->
      let f = Py.Callable.to_function_with_keywords f in
      ignore @@ f [| Py.String.of_string path |] []
