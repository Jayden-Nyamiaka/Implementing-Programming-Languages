module StringMap = Map.Make(String)

type ('a, 'b) t = 
  {
    locals    : 'a StringMap.t;
    globals   : 'a StringMap.t;
    functions : 'b StringMap.t
  }

let find_helper l name map =
  try 
    StringMap.find name map
  with Not_found -> 
    Error.name_err l name

let lookup_var env l name =
  if StringMap.mem name env.locals then
    find_helper l name env.locals
  else
    find_helper l name env.globals

let lookup_fun env l name = find_helper l name env.functions

let bind_global env name v =
  { env with globals = StringMap.add name v env.globals }

let combine env1 env2 = {env1 with globals = env2.globals}

let set env l name v =
  if StringMap.mem name env.locals then
    {env with locals = StringMap.add name v env.locals}
  else if StringMap.mem name env.globals then
    {env with globals = StringMap.add name v env.globals}
  else
    Error.name_err l name

let assoc_list_to_map l =
  List.fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty l

let bind_locals env l = {env with locals = assoc_list_to_map l}

let bind_fun env name v =
  { env with functions = StringMap.add name v env.functions }

let empty = 
  {
    locals    = StringMap.empty;
    globals   = StringMap.empty;
    functions = StringMap.empty
  }

