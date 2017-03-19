-- -*- mode: notion-wm; -*-

emacs = {}

emacs.verbose = true

function emacs.completion_candidates(str)
  local env = _ENV or getfenv()
  local completions = mod_query.do_complete_lua(env, str)
  return table.concat(completions, " ")
end

-- Pretty print a notion object.
-- type(obj) == userdata and obj.__typename exists
function emacs.pp_notion(obj, short)

  if not obj then
    return "nil"
  end

  if short then
    return obj.__typename.." "..tostring(obj)
  end

  local repr =        "Type:   "..obj.__typename
  local sep = "\n  "
  if obj.name then
    repr = repr..sep.."Name:   "..obj:name()
  else
    repr = repr..sep.."Id:     "..tostring(obj)
  end
  if obj.parent then
    repr = repr..sep..  "Parent: "..emacs.pp_notion(obj:parent(), true)
  end

  return repr
end

-- _Attempts_ to pretty print `obj`
function emacs.pprint(obj)
  local obj_type = type(obj)
  if obj_type == "table" then
    return table_to_string(obj)
  elseif obj_type == "userdata" and obj.__typename then
    return emacs.pp_notion(obj)
  else
    return obj
  end
end

function parse_fname(fname)
  local dot = string.find(fname, "[.:]", nil)
  local tabpart, funpart
  if dot then
    tabpart = string.sub(fname, 1, dot-1)
    funpart = string.sub(fname, dot+1)
  else
    funpart = fname
  end
  return funpart, tabpart, dot and string.sub(fname, dot, dot)
end


function emacs.smart_loadstring(lua_code)
  local fn, err = loadstring("return "..lua_code)
  if not fn then
    fn, err = loadstring(lua_code)
  end
  return fn, err
end

function emacs.eval(lua_code)
  if emacs.verbose then 
    debug.print_line(lua_code)
  end
  fn, err = emacs.smart_loadstring(lua_code)
  if err then
    error(err)
  else
    local result = fn()
    return emacs.pprint(result)
  end
end

-- Walk upwards in the metatable tree looking for the owner of the function
-- Returns the _name_ (string)
-- notion class system specific, but might be equivalent to replace .__parent with get
-- metatable?
function emacs.function_owner(tab, funstr)
  if not tab or not tab.__typename then
    return nil -- we only understand notion classes
  end
  while tab do
    if rawget(tab, funstr) then
      return tab
    else
      tab = tab.__parentclass
    end
  end
end


function emacs.canonical_funcname(fname)
  -- fname: WMPlex.geom or 
  funpart, tabpart = parse_fname(fname)

  if not tabpart then
    return fname
  end

  local tab = _ENV[tabpart]
  if type(tab) == "userdata" then
    tab = getmetatable(tab)
    if not tab then
      return fname
    end
    tab = tab.__index
  end

  local owner = emacs.function_owner(tab, funpart)

  if not owner or not owner.__typename then
    return fname
  end

  return owner.__typename.."."..funpart
end

function introspect_function(fun)
  -- http://stackoverflow.com/a/29246308/1517969
  local args = {}
  local info = nil
  local hook = debug.gethook()

  local argHook = function( ... )
    local cur_frame = debug.getinfo(3)
    if not cur_frame or 'pcall' ~= cur_frame.name then return end

    info = debug.getinfo(2)

    for i = 1, 20 do
      local name, value = debug.getlocal(2, i)
      if '(*temporary)' == name then
        debug.sethook(hook)
        error('')
        return
      end
      table.insert(args,name)
    end
  end
  
  debug.sethook(argHook, "c")
  pcall(fun)
  
  return args, info
end

function emacs.eldoc(function_name)
  if string.find(function_name, "[{\"(]") then
    return -- weird stuff in name, bail (the name would've been eval'ed later)
  end

  local canonical_fname = emacs.canonical_funcname(function_name)

  local get_func, err = loadstring("return "..canonical_fname)
  if err then
    return nil
  end

  local ok, func = pcall(get_func)
  if not ok or  type(func) ~= "function" then
    return nil
  end

  local funpart, tabpart, sep = parse_fname(function_name)

  local args, info = introspect_function(func)
  local is_native = info.what == "C"

  if not next(args) then
    table.insert(args, "...?")
  end

  if info.isvararg and not is_native then
    table.insert(args, "...")
  end

  if sep == ":" and canonical_fname ~= function_name then
    if is_native then table.insert(args, 1, "?") end
    args[1] = args[1].."="..tabpart
  end

  local args_str = table.concat(args, ", ")
  local eldoc = canonical_fname.." ("..args_str..")"

  local native_marker = ""
  if is_native then
    native_marker = "   ".."["..info.what.."]"
  end

  return eldoc..native_marker
end

function emacs.defined_at(function_name)
  if string.find(function_name, "[{\"(]") then
    return -- weird stuff in name, bail (the name would've been eval'ed later)
  end

  local get_func, err = loadstring("return "..function_name)
  if err then
    return nil
  end

  local ok, func = pcall(get_func)
  if not ok or  type(func) ~= "function" then
    return nil
  end

  local args, info = introspect_function(func)
  if info.source and string.sub(info.source, 1, 1) == "@" then
    return "("..'"'..string.sub(info.source, 2)..'"' .. " " .. tostring(info.linedefined)..")"
  end

  return nil

end
