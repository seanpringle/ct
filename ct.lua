getmetatable("").__mod = function(s, tab)
	return (s:gsub('($%b{})', function(w) return tostring(tab[w:sub(3, -2)]) or tostring(w) end))
end

local sequence = 1

local nextId = function()
	local id = sequence
	sequence = sequence+1
	return id
end

local mtcopy = function(t, e)
	local c = {
		__tostring = t.__tostring,
		__index = e,
	}
	for k,v in pairs(t.__index) do
		if not e[k] then
			e[k] = v
		end
	end
	return c
end

local libraries = {}

local symbolMT = {
	__tostring = function(sym)
		return sym.lib and (sym.lib.name .."_".. sym.name) or sym.name
	end,

	__index = {
		resolve = function(sym)
			for name,lib in pairs(libraries) do
				if name == sym.lib then
					sym.lib = lib
					return
				end
			end
			assert(false, "symbol has unknown library "..sym.lib..":"..sym.name)
		end,
	},
}

local opaqueMT = {
	__tostring = function(op)
		return tostring(op.sym)
	end,
	__index = {
		declare = function(op)
			return "typedef ${ctype} ${sym};" % op
		end,
		define = function(op)
			return "// "..tostring(op)
		end,
	},
}

local structMT = {
	__tostring = function(st)
		return tostring(st.sym)
	end,

	__index = {
		field = function(st, type, name, cells)
			local field = setmetatable({ cells = cells }, fieldMT)
			if type == "embed" then
				field.esym = st.lib:symbolref(name)
				field.embed = true
				field.sym = field.esym
				field.name = field.esym.name
			else
				field.sym = st.lib:symbolref(type)
				field.name = name
			end
			st.fields[field.name] = field
			st.order[#st.order+1] = field.name
		end,

		embed = function(st)
			assert(not st.embedding, "embedded struct loop "..tostring(st))
			st.embedding = true
			local rfields = {}
			local efields = {}
			for n,f in pairs(st.fields) do
				if f.embed then
					assert(type(f.esym.lib) == "table")
					assert(type(f.esym.lib.structs) == "table")
					assert(type(f.esym.lib.structs[f.name]) == "table", f.name)
					local sst = f.esym.lib.structs[f.name]
					for sn,sf in pairs(sst.fields) do
						assert(n == sn or not st.fields[sn], "duplicate embedded field "..sn)
						if sf.embed then
							sst:embed()
						end
					end
					for i,sn in ipairs(sst.order) do
						efields[#efields+1] = sst.fields[sn]
					end
					rfields[#rfields+1] = f
				end
			end
			for i,f in pairs(rfields) do
				st.fields[f.name] = nil
				for j,n in ipairs(st.order) do
					if n == f.name then
						table.remove(st.order, j)
						break
					end
				end
			end
			for i,f in ipairs(efields) do
				st.fields[f.name] = f
				st.order[#st.order+1] = f.name
			end
			st.embedding = false
		end,

		declareConcrete = function(st)
			return "struct _${name}_concrete;\n" % { name = st.sym }
				.. "typedef struct _${name}_concrete ${name}_concrete;\n" % { name = st.sym }
		end,

		declare2Concrete = function(st)
			local fields = "\tint32_t _type;\n\tuint32_t _flag;\n"
			for i,n in ipairs(st.order) do
				local f = st.fields[n]
				if f.cells == 0 then
					fields = fields .. "\t${type} ${name};\n" % { type = f.sym, name = f.name }
				else
					fields = fields .. "\t${type} ${name}[${cells}];\n" % { type = f.sym, name = f.name, cells = f.cells }
				end
			end
			return "struct _${name}_concrete {\n${fields}};\n" % { fields = fields, name = st.sym }
		end,

		declareInterface = function(st)
			return "struct _${name}_interface;\n" % { name = st.sym }
				.. "typedef struct _${name}_interface ${name};\n" % { name = st.sym }
		end,

		declare2Interface = function(st)
			local fields = "\tvoid* _concrete;\n"
			for i,n in ipairs(st.order) do
				local f = st.fields[n]
				fields = fields .. "\t${type} *${name};\n" % { type = f.sym, name = f.name }
			end
			return "struct _${name}_interface {\n${fields}};\n" % { fields = fields, name = st.sym }
		end,

		declare2Cast = function(st)
			local out = {}
			out[#out+1] = "#define ${sym}_cast(i) ({" % st
			out[#out+1] = "\t__typeof__ (i) _i = (i);" % st
			out[#out+1] = "\t(${sym}) {" % st
			out[#out+1] = "\t\t._concrete = _i._concrete,"
			for i,name in ipairs(st.order) do
				local field = st.fields[name]
				out[#out+1] = "\t\t.${name} = _i.${name}," % field
			end
			out[#out+1] = "\t};" % st
			out[#out+1] = "})" % st
			return table.concat(out, "\\\n").."\n"
		end,

		declare2Local = function(st)
			local out = {}
			out[#out+1] = "#define ${sym}_local(l) ({" % st
			out[#out+1] = "\t${sym}_concrete *_l = (l);" % st
			out[#out+1] = "\tmemset(_l, 0, sizeof(${sym}_concrete));" % st
			out[#out+1] = "\t_l->_type = ${kind};" % st
			out[#out+1] = "\t_l->_flag = isLocal;" % st
			out[#out+1] = "\t(${sym}) {" % st
			out[#out+1] = "\t\t._concrete = _l,"
			for i,name in ipairs(st.order) do
				local field = st.fields[name]
				if field.cells == 0 then
					out[#out+1] = "\t\t.${name} = &_l->${name}," % field
				else
					out[#out+1] = "\t\t.${name} = _l->${name}," % field
				end
			end
			out[#out+1] = "\t};" % st
			out[#out+1] = "})" % st
			return table.concat(out, "\\\n").."\n"
		end,

		declare2Temp = function(st)
			return "${sym} ${sym}_temp();\n" % st
		end,

		declare2Pool = function(st)
			return "${sym} ${sym}_heap();\n" % st
		end,

		declare = function(st)
			return st:declareInterface()
				.."\n".. st:declareConcrete()
		end,

		declare2 = function(st)
			return st:declare2Interface()
		end,

		declare3 = function(st)
			return st:declare2Concrete()
				.."\n".. st:declare2Cast()
				.."\n".. st:declare2Local()
				.."\n".. st:declare2Temp()
				.."\n".. st:declare2Pool()
		end,

		defineTemp = function(st)
			local out = {}
			out[#out+1] = "${sym} ${sym}_temp() {" % st
			out[#out+1] = "\t${sym}_concrete *_t = allot(sizeof(${sym}_concrete));" % st
			out[#out+1] = "\t_t->_type = ${kind};" % st
			out[#out+1] = "\t_t->_flag = isTemp;" % st
			out[#out+1] = "\treturn (${sym}) {" % st
			out[#out+1] = "\t\t._concrete = _t,"
			for i,name in ipairs(st.order) do
				local field = st.fields[name]
				if field.cells == 0 then
					out[#out+1] = "\t\t.${name} = &_t->${name}," % field
				else
					out[#out+1] = "\t\t.${name} = _t->${name}," % field
				end
			end
			out[#out+1] = "\t};" % st
			out[#out+1] = "}" % st
			return table.concat(out, "\n").."\n"
		end,

		definePool = function(st)
			local out = {}
			out[#out+1] = "${sym} ${sym}_heap() {" % st
			out[#out+1] = "\t${sym}_concrete *_p = calloc(1, sizeof(${sym}_concrete));" % st
			out[#out+1] = "\t_p->_type = ${kind};" % st
			out[#out+1] = "\t_p->_flag = isPool;" % st
			out[#out+1] = "\treturn (${sym}) {" % st
			out[#out+1] = "\t\t._concrete = _p,"
			for i,name in ipairs(st.order) do
				local field = st.fields[name]
				if field.cells == 0 then
					out[#out+1] = "\t\t.${name} = &_p->${name}," % field
				else
					out[#out+1] = "\t\t.${name} = _p->${name}," % field
				end
			end
			out[#out+1] = "\t};" % st
			out[#out+1] = "}" % st
			return table.concat(out, "\n").."\n"
		end,

		define = function(st)
			return st:defineTemp() .."\n".. st:definePool()
		end,
	},
}

local exprMT = {
	__tostring = function(e)
		return e.raw
	end,

	__index = {
		define = function(e)
			return e.raw
		end,
	},
}

local exprGroupMT = {
	__tostring = function(e)
		return "group"
	end,

	__index = {
		define = function(e)
			local parts = {}
			for i,e in ipairs(e) do
				parts[#parts+1] = e:define()
			end
			return table.concat(parts)
		end,
	},
}

local exprCallMT = {
	__tostring = function(e)
		return e.name
	end,

	__index = {
		define = function(e)
			assert(type(e.sym.lib) == "table")
			assert(type(e.sym.lib.funcs[e.sym.name]) == "table", e.sym.name)
			local fn = e.sym.lib.funcs[e.sym.name]
			local args = {}
			for i,a in ipairs(e.args) do
				assert(fn.args[i] or (fn.variadic and i > #fn.args))
				local fa = fn.args[i]
				if i <= #fn.args and fa and fa.tsym.lib and not fa.tsym.lib.opaques[fa.tsym.name] then
					args[#args+1] = "${tsym}_cast(${arg})" % { tsym = fa.tsym, arg = a:define() }
				else
					args[#args+1] = a:define()
				end
			end
			return "${sym}(${args})" % { sym = e.sym, args = table.concat(args,", ") }
		end,
	},
}

local exprAllotMT = {
	__tostring = function(e)
		return e.tsym
	end,

	__index = {
		define = function(e)
			if e.location == "local" then
				return "${tsym}_local(&${concrete})" % e
			end
			return "${tsym}_${location}()" % e
		end,
	},
}

local exprPathMT = {
	__tostring = function(e)
		return "path"
	end,

	__index = {

		field = function(e,name)
			e[#e+1] = "."..name
			e[#e+1] = "[0]"
		end,

		index = function(e,cell)
			e[#e] = "["..cell.."]"
		end,

		define = function(e)
			return table.concat(e)
		end,
	},
}

local stmtMT = {
	__tostring = function(sm)
		return tostring(sm.sym)
	end,

	__index = {

		C = function(sm, rawC)
			local ex = sm.iexpr
			ex[#ex+1] = setmetatable({raw = rawC}, exprMT)
		end,

		number = function(sm, ex, n)
			ex[#ex+1] = setmetatable({raw = n}, exprMT)
		end,

		string = function(sm, ex, s)
			ex[#ex+1] = setmetatable({raw = '"'..s..'"'}, exprMT)
		end,

		operator = function(sm, ex, n)
			ex[#ex+1] = setmetatable({raw = n}, exprMT)
		end,

		reference = function(sm, ex, name)
			ex[#ex+1] = setmetatable({raw = name}, exprMT)
		end,

		path = function(sm, ex, name)
			ex[#ex+1] = setmetatable({name}, exprPathMT)
			return ex[#ex]
		end,

		call = function(sm, ex, name, args)
			local sym = sm.fn.lib:symbolref(name)
			ex[#ex+1] = setmetatable({sym = sym, args = args}, exprCallMT)
		end,

		callfield = function(sm, ex, name, args)
			ex[#ex+1] = setmetatable({sym = name, args = args}, exprCallMT)
		end,

		struct = function(sm, type, name)
			local tsym = sm.fn.lib:symbolref(type)
			local concrete = nil
			if sm.location == "local" then
				concrete = "_${name}" % { name = name }
				sm.fn:var("${type}_concrete" % { type = type}, concrete)
			end
			sm.oexpr[#sm.oexpr+1] = setmetatable({raw = name}, exprMT)
			sm.iexpr[#sm.iexpr+1] = setmetatable({tsym = tsym, location = sm.location, concrete = concrete }, exprAllotMT)
		end,

		define = function(sm)

			local oexpr = {}
			for i,e in ipairs(sm.oexpr) do
				oexpr[#oexpr+1] = e:define()
			end

			local iexpr = {}
			for i,e in ipairs(sm.iexpr) do
				iexpr[#iexpr+1] = e:define()
			end

			if sm.keyword then
				if #oexpr == 1 and #iexpr > 0 then
					return "${keyword} (${iexpr}) ${oexpr}\n" % {
						keyword = sm.keyword,
						oexpr = table.concat(oexpr),
						iexpr = table.concat(iexpr),
					}
				end
				if #oexpr > 0 and #iexpr > 0 then
					return "${keyword} (${iexpr}) {\n\t${oexpr}\t}\n" % {
						keyword = sm.keyword,
						oexpr = table.concat(oexpr, "\t"),
						iexpr = table.concat(iexpr),
					}
				end
				if #oexpr == 1 then
					return "${keyword} ${oexpr}\n" % {
						keyword = sm.keyword,
						oexpr = table.concat(oexpr, "\t"),
					}
				end
				if #oexpr > 0 then
					return "${keyword} {\n\t${oexpr}\t}\n" % {
						keyword = sm.keyword,
						oexpr = table.concat(oexpr, "\t"),
					}
				end
				if #iexpr > 0 then
					return "${keyword} ${iexpr};\n" % {
						keyword = sm.keyword,
						iexpr = table.concat(iexpr),
					}
				end
				return "${keyword};" % sm
			end

			if #oexpr > 0 then
				return "${oexpr} = ${iexpr};\n" % {
					oexpr = table.concat(oexpr),
					iexpr = table.concat(iexpr),
				}
			end

			if #iexpr > 0 then
				return "${iexpr};\n" % {
					iexpr = table.concat(iexpr),
				}
			end

			assert(false, "empty statement")
		end,
	},
}

local varMT = {
	__tostring = function(va)
		return va.sym
	end,

	__index = {

		declare = function(va)
			return "${tsym} ${sym};" % va
		end,

		define = function(va)
			return "${tsym} ${sym}" % va
		end,
	},
}

local funcMT = {
	__tostring = function(fn)
		return tostring(fn.sym)
	end,

	__index = {

		arg = function(fn, type, name)
			local tsym = fn.lib:symbolref(type)
			local arg = setmetatable({
				fn = fn,
				sym = name,
				tsym = tsym,
				init = false,
			}, varMT)
			fn.args[#fn.args+1] = arg
			fn.vars[name] = arg
			return arg
		end,

		var = function(fn, type, name)
			local tsym = fn.lib:symbolref(type)
			local var = setmetatable({
				fn = fn,
				sym = name,
				tsym = tsym,
				init = true,
			}, varMT)
			fn.vars[name] = var
			return var
		end,

		stmt = function(fn)
			local stmt = setmetatable({
				fn = fn,
				oexpr = {},
				iexpr = {},
				location = "local",
			}, stmtMT)
			fn.stmts[#fn.stmts+1] = stmt
			return stmt
		end,

		istmt = function(fn)
			local stmt = setmetatable({
				fn = fn,
				oexpr = {},
				iexpr = {},
				location = "local",
			}, stmtMT)
			local tmp = "_index"..#fn.stmts
			table.insert(fn.stmts, #fn.stmts, stmt)
			fn:var("int", tmp)
			stmt:reference(stmt.oexpr,tmp)
			return stmt, tmp
		end,

		nop = function(fn)
			local stmt = fn:stmt()
			stmt:C("nop()")
		end,

		variadic = function(fn)
			fn.isvariadic = true
		end,

		declare = function(fn)
			local args = {}
			for i,a in ipairs(fn.args) do
				args[#args+1] = "${tsym} ${sym}" % a
			end

			if fn.isvariadic then
				args[#args+1] = "..."
			end

			return "${type} ${name}(${args});" % {
				type = fn.tsym,
				name = fn.sym,
				args = table.concat(args, ", "),
			}
		end,

		define = function(fn)

			local args = {}
			for i,a in ipairs(fn.args) do
				args[#args+1] = a:define()
			end

			if fn.isvariadic then
				args[#args+1] = "..."
			end

			local vars = {}
			for n,v in pairs(fn.vars) do
				if v.init then
					vars[#vars+1] = "\t"..v:define()..";\n"
				end
			end

			local stmts = {}
			for i,s in ipairs(fn.stmts) do
				stmts[#stmts+1] = "\t"..s:define()
			end

			return "${type} ${name}(${args}) {\n${vars}${stmts}}\n" % {
				type = fn.tsym,
				name = fn.sym,
				args = table.concat(args, ", "),
				vars = table.concat(vars),
				stmts = table.concat(stmts),
			}
		end,
	},
}

local libMT = {
	__tostring = function(lib)
		return lib.name
	end,
	__index = {

		link = function(lib,name)
			lib.links[#lib.links+1] = { name = name }
		end,

		C = function(lib,rawC)
			lib.Cchunks[#lib.Cchunks+1] = rawC
		end,

		Cinclude = function(lib,inc)
			lib.Cincludes[#lib.Cincludes+1] = inc
		end,

		symbol = function(lib, name)
			assert(not name:match(":"), "symbol definition outside library "..name)
			assert(not lib.symbols[name], "duplicate symbol "..name)
			local sym = setmetatable({
				lib = lib.name,
				name = name,
			}, symbolMT)
			lib.symbols[name] = sym
			return sym
		end,

		symbolref = function(lib, name)
			local lname = lib.name

			if name:match(":") then
				lname = name:gsub(":.*", "")
				name = name:gsub(".*:", "")
			end

			if not lib.symbolrefs[name] then

				local sym = setmetatable({
					lib = lname,
					name = name,
				}, symbolMT)

				lib.symbolrefs[name] = sym
			end

			return lib.symbolrefs[name]
		end,

		struct = function(lib, name)
			local sym = lib:symbol(name)
			local st = setmetatable({
				lib = lib,
				sym = sym,
				fields = {},
				order = {},
				kind = nextId(),
			}, structMT)
			lib.structs[name] = st
			--lib.structs[name.."_concrete"] = st
			return st
		end,

		opaque = function(lib, ctype, name)
			local sym = lib:symbol(name)
			local op = setmetatable({
				lib = lib,
				sym = sym,
				ctype = ctype,
				kind = nextId(),
			}, opaqueMT)
			lib.opaques[name] = op
		end,

		func = function(lib, type, name)
			local tsym = lib:symbolref(type)
			local nsym = lib:symbol(name)
			local fn = setmetatable({
				lib = lib,
				sym = nsym,
				tsym = tsym,
				args = {},
				vars = {},
				stmts = {},
				kind = nextId(),
			}, funcMT)
			lib.funcs[name] = fn
			return fn
		end,

		resolve = function(lib)
			for i,sym in pairs(lib.symbols) do
				sym:resolve()
			end
			for i,sym in pairs(lib.symbolrefs) do
				sym:resolve()
			end
			for i,sym in pairs(lib.symbols) do
				print(sym)
				lib:import(sym)
			end
			for i,sym in pairs(lib.symbolrefs) do
				print(sym)
				lib:import(sym)
			end
		end,

		locate = function(lib,name)
			local hits = {}
			for i,use in ipairs(lib.links) do
				local ulib = libraries[use.name]
				if ulib.opaques[name]
					or ulib.structs[name]
					or ulib.funcs[name]
				then
					hits[#hits+1] = ulib
				end
			end
			return hits
		end,

		import = function(lib,sym)
			if sym.lib == lib then
				local name = sym.name
				-- local interfaces create concrete variable symbols
				if name:match("_concrete$") then
					name = name:gsub("_concrete$", "")
				end
				if lib.opaques[name]
					or lib.structs[name]
					or lib.funcs[name]
				then
					return
				end
				local hits = lib:locate(name)
				assert(#hits == 1, "symbol "..name.." is unknown or ambiguous")
				sym.lib = hits[1]
				print(sym.lib, "auto", name)
			end
		end,

		prepare = function(lib)
			lib.vars = lib.init.vars
			for n,v in pairs(lib.vars) do
				v.init = false
				v.extern = not v.tsym.name:match("_concrete$")
			end
			--lib.init.vars = {}
		end,

		embed = function(lib)
			for i,st in pairs(lib.structs) do
				st:embed()
			end
		end,

		output = function(lib)
			local h = io.open("build/"..lib.name..".h", "w+")
			local c = io.open("build/"..lib.name..".c", "w+")

			h:write("#ifndef _H_${name}\n" % lib)
			h:write("#define _H_${name}\n" % lib)

			c:write("#include \"common.h\"\n")
			c:write("#include \"rt.h\"\n" % lib)

			if lib.name ~= "rt" then
				c:write("#include \"${name}.h\"\n" % lib)
			end

			for i,use in pairs(lib.links) do
				if use.name ~= "rt" then
					h:write("#include \"${name}.h\"\n" % use)
				end
			end

			h:write("\n")
			c:write("\n")

			for i,obj in pairs(lib.Cincludes) do
				h:write(obj.."\n")
			end

			for i,obj in pairs(lib.opaques) do
				h:write(obj:declare().."\n")
			end

			for i,obj in pairs(lib.opaques) do
				c:write(obj:define().."\n")
			end

			for i,obj in pairs(lib.structs) do
				h:write(obj:declare().."\n")
			end

			for i,obj in pairs(lib.structs) do
				h:write(obj:declare2().."\n")
			end

			for i,obj in pairs(lib.structs) do
				h:write(obj:declare3().."\n")
			end

			for i,obj in pairs(lib.structs) do
				c:write(obj:define().."\n")
			end

			for i,obj in pairs(lib.vars) do
				if obj.extern then
					h:write("extern "..obj:declare().."\n")
				end
			end

			for i,obj in pairs(lib.funcs) do
				h:write(obj:declare().."\n")
			end

			c:write("// extern\n")
			for i,obj in pairs(lib.vars) do
				c:write(obj:declare().."\n")
			end
			c:write("\n")

			for i,obj in pairs(lib.Cchunks) do
				c:write(obj.."\n")
			end

			for i,obj in pairs(lib.funcs) do
				c:write(obj:define().."\n")
			end

			h:write("#endif")

			h:close()
			c:close()
		end,
	},
}

local library = function(lib)
	if libraries[lib] then
		return libraries[lib]
	end
	local l = setmetatable({
		name = lib,
		symbols = {},
		symbolrefs = {},
		opaques = {},
		structs = {},
		funcs = {},
		links = {},
		vars = {},
		Cchunks = {},
		Cincludes = {},
	}, libMT)
	libraries[lib] = l
	l.init = l:func("void", "init")
	l:link("rt",true)
	return l
end

local function newParser(file)
	return setmetatable({
		handle = assert(io.open(file, "r")),
		queue = {},
		eof = false,
		typePattern = "[a-zA-Z][a-zA-Z0-9_:]*",
		namePattern = "[a-zA-Z][a-zA-Z0-9_]*",
		callPattern = "[a-zA-Z][a-zA-Z0-9_:]*",
		operators = {
			["+"] = "+",
			["-"] = "-",
			["*"] = "*",
			["/"] = "/",
			["%"] = "%",
			["<="] = "<=",
			["<"] = "<",
			[">="] = ">=",
			[">"] = ">",
			["=="] = "==",
			["&"] = "&",
			["and"] = "&&",
			["|"] = "|",
			["or"] = "||",
			["~"] = "~",
			["!"] = "!",
		},
	}, { __index = {
		done = function(p)
			p.handle:close()
		end,

		buffer = function(p, n)
			while #p.queue < n do
				local c = p.handle:read(1)
				if c then
					table.insert(p.queue, c)
				else
					p.eof = true
					break
				end
			end
			return math.min(#p.queue, n)
		end,

		bufferLine = function(p)
			while not p.eof and p.queue[#p.queue] ~= "\n" do
				p:buffer(#p.queue+1)
			end
		end,

		peek = function(p, n)
			n = p:buffer(n or 1)
			local s = nil
			for i = 1,n do
				s = (s or "") .. p.queue[i]
			end
			return s
		end,

		peekLine = function(p)
			p:bufferLine()
			local s = nil
			for i = 1,#p.queue do
				s = (s or "") .. p.queue[i]
				if p.queue[i] == "\n" then
					break
				end
			end
			return s
		end,

		take = function(p, n)
			local c = p:peek(n)
			if c then
				for i = 1,c:len() do
					table.remove(p.queue, 1)
				end
			end
			return c
		end,

		takeLine = function(p)
			local s = p:peekLine()
			for i = 1,#p.queue do
				p.queue[i] = nil
			end
			return s
		end,

		skip = function(p)
			while p:peek() and (p:peek():match("%s") or p:peek(2) == "//" or p:peek(2) == "/*") do
				while p:peek() and p:peek():match("%s") do
					p:take()
				end
				if p:peek(2) == "//" then
					while p:peek() and p:peek() ~= "\n" do
						p:take()
					end
				end
				if p:peek(2) == "/*" then
					while p:peek() and p:peek(2) ~= "*/" do
						p:take()
					end
				end
			end
		end,

		word = function(p)
			p:skip()
			s = nil
			while p:peek() and not p:peek():match("%s") do
				s = (s or "") .. p:take()
			end
			p:skip()
			assert(s, "expected word "..p:peekLine())
			return s
		end,

		peekName = function(p)
			p:skip()
			return p:peek() and p:peek():match("[a-zA-Z]")
		end,

		name = function(p)
			assert(p:peekName(), "expected name "..p:peekLine())
			s = ""
			while p:peek() and p:peek():match("[a-zA-Z0-9_:]") do
				s = s .. p:take()
			end
			p:skip()
			return s
		end,

		peekOperator = function(p)
			p:skip()
			for k,v in pairs(p.operators) do
				if p:peek(k:len()) == k then
					return true, k, v
				end
			end
		end,

		operator = function(p)
			local ok, key, val = p:peekOperator(p)
			if ok then
				p:take(key:len())
			end
			return val
		end,

		peekNumber = function(p)
			p:skip()
			return p:peek():match("%d")
		end,

		number = function(p)
			assert(p:peekNumber())
			s = ""
			while p:peek() and p:peek():match("[0-9]") do
				s = s .. p:take()
			end
			p:skip()
			return tonumber(s), s
		end,

		skipComma = function(p)
			p:skip()
			if p:peek() == "," then
				p:take()
			end
			p:skip()
		end,

		struct = function(p)
			assert(p:name() == "struct", "expected struct")
			assert(p:peekName(), "expected struct name")
			local name = p:name()
			assert(p:take() == "{", "expected struct block")
			local struct = p.library:struct(name)
			while p:peek() and not (p:peek() == "}") do
				local ftype = p:name()
				local fname = p:name()
				local fcells = 0
				if p:peek() == "[" then
					p:take()
					fcells = p:number()
					assert(p:take() == "]")
					p:skip()
				end
				struct:field(ftype, fname, fcells)
			end
			assert(p:take() == "}")
			p:skip()
		end,

		opaque = function(p)
			assert(p:name() == "opaque", "expected opaque")
			local ctype = p:word()
			local oname = p:name()
			p.library:opaque(ctype, oname)
		end,

		init = function(p)
			p:stmt(p.library.init)
			p:skip()
		end,

		init2 = function(p)
			p:skip()
			p:name()
			p:name()
			assert(p:take(2) == "()", "init can't receive arguments")
			p:skip()
			assert(p:peek() == "{", "expected function block")
			p:block(p.library.init)
			p:skip()
		end,

		func = function(p)
			p:skip()
			local ftype = p:name()
			local fname = p:name()
			assert(ftype, "expected function return type")
			assert(fname, "expected function name")
			local func = p.library:func(ftype, fname)
			assert(p:take() == "(", "expected start of function arguments")
			local ltype = nil
			while p:peek() and not (p:peek() == ")") do
				p:skip()
				if p:peek(3) == "..." then
					p:take(3)
					p:skip()
					func:variadic()
					break
				end
				local atype = p:name()
				local aname = nil
				if not p:peekName() then
					aname = atype
					atype = ltype
				else
					aname = p:name()
				end
				func:arg(atype, aname)
				p:skipComma()
				ltype = atype
			end
			assert(p:take() == ")", "expected end of function arguments")
			p:skip()
			assert(p:peek() == "{", "expected function block")
			p:block(func)
			p:skip()
		end,

		block = function(p,func)
			p:skip()
			if p:peek(2) == "{{" then
				p:take(2)
				local rawC = ""
				while p:peek() and not (p:peek(2) == "}}") do
					rawC = rawC .. p:take()
				end
				assert(p:take(2) == "}}", p:peekLine())
				p:skip()
				func:stmt():C(rawC)
				return
			end
			local n = 0
			if p:peek() == "{" then
				p:take()
				p:skip()
				while p:peek() and not (p:peek() == "}") do
					p:stmt(func)
					n = n+1
				end
				assert(p:take() == "}")
			else
				p:stmt(func)
				n = 1
			end
			-- a block is never empty
			if n == 0 then
				func:nop()
			end
			p:skip()
		end,

		stmt = function(p,func,location)
			p:skip()
			local stmt = func:stmt()

			if location then
				stmt.location = location
			end

			if p:peekLine():match("^local[%s%c]") then
				p:name()
				stmt.location = "local"
			end

			if p:peekLine():match("^temp[%s%c]") then
				p:name()
				stmt.location = "temp"
			end

			if p:peekLine():match("^heap[%s%c]") then
				p:name()
				stmt.location = "heap"
			end

			if p:peekLine():match("^region[%s%c]") then
				p:take(6)
				p:skip()
				stmt:C("{ rt_region_start()")
				p:block(func)
				func:stmt():C("rt_region_end(); }")
				return
			end

			if p:peekLine():match("^return[%s%c]") then
				p:take(6)
				stmt.keyword = "return"
				p:expr(stmt,stmt.iexpr)
				return
			end

			if p:peekLine():match("^break[%s%c]") then
				p:take(5)
				stmt.keyword = "break"
				return
			end

			if p:peekLine():match("^continue[%s%c]") then
				p:take(8)
				stmt.keyword = "continue"
				return
			end

			if p:peekLine():match("^do[%s%c]") then
				p:take(2)
				stmt.keyword = "for(;;)"
				local start = #func.stmts
				p:block(func)
				while start < #func.stmts do
					stmt.oexpr[#stmt.oexpr+1] = table.remove(func.stmts,start+1)
				end
				return
			end

			if p:peekLine():match("^if[%s%c]") then
				p:take(2)
				stmt.keyword = "if"
				p:expr(stmt,stmt.iexpr)
				local start = #func.stmts
				p:block(func)
				while start < #func.stmts do
					stmt.oexpr[#stmt.oexpr+1] = table.remove(func.stmts,start+1)
				end
				return
			end

			if p:peekLine():match("^else[%s%c]") then
				p:take(4)
				p:skip()
				if p:peekLine():match("^if[%s%c]") then
					p:take(2)
					stmt.keyword = "else if"
					p:expr(stmt,stmt.iexpr)
					local start = #func.stmts
					p:block(func)
					while start < #func.stmts do
						stmt.oexpr[#stmt.oexpr+1] = table.remove(func.stmts,start+1)
					end
				else
					stmt.keyword = "else"
					local start = #func.stmts
					p:block(func)
					while start < #func.stmts do
						stmt.oexpr[#stmt.oexpr+1] = table.remove(func.stmts,start+1)
					end
				end
				return
			end

			if p:peekLine():match("^${tp}%s+${np}%s*=" % { tp = p.typePattern, np = p.namePattern }) then
				local vtype = p:name()
				while p:peekLine():match("^${np}%s*=" % { np = p.namePattern }) do
					if not stmt then
						stmt = func:stmt()
					end
					local vname = p:name()
					func:var(vtype,vname)
					assert(p:take() == "=")

					p:skip()
					if p:peek() == "{" then
						p:take()
						p:skip()
						stmt:struct(vtype,vname)
						while p:peek() and not (p:peek() == "}") do
							assert(p:peekLine():match("${np}%s*=" % { np = p.namePattern }))
							local fname = p:name()
							assert(p:take() == "=")
							stmt = func:stmt()
							stmt:reference(stmt.oexpr,"*${vname}.${fname}" % { vname = vname, fname = fname })
							p:expr(stmt,stmt.iexpr)
							p:skipComma()
						end
						assert(p:take() == "}")
					else
						stmt:reference(stmt.oexpr,vname)
						p:expr(stmt,stmt.iexpr)
					end
					p:skipComma()
					stmt = nil
				end
				return
			end

			p:expr(stmt,stmt.iexpr)

			if p:peek() == "=" then
				p:take()
				stmt.oexpr = stmt.iexpr
				stmt.iexpr = {}
				p:expr(stmt,stmt.iexpr)
			end
		end,

		expr = function(p,stmt,queue)
			p:skip()

			local step = function()
				p:skip()

				if p:peek() == "(" then
					stmt:operator(queue,p:take())
					while p:peek() and not (p:peek() == ")") do
						p:expr(stmt,queue)
						p:skip()
					end
					assert(p:peek() == ")")
					stmt:operator(queue,p:take())
					return
				end

				if p:peekOperator() then
					stmt:operator(queue,p:operator())
					return
				end

				if p:peekName() then
					local name = p:name()

					if p:peek() == "(" then
						p:take()
						local args = {}
						while p:peek() and not (p:peek() == ")") do
							local group = setmetatable({}, exprGroupMT)
							p:expr(stmt,group)
							args[#args+1] = group
							p:skipComma()
						end
						assert(p:take() == ")")
						stmt:call(queue,name,args)
					else
						local path = stmt:path(queue, name)
						while p:peek() == "." or p:peek() == "[" do
							if p:peek() == "." then
								p:take()
								path:field(p:name())
							end
							if p:peek() == "[" then
								p:take()
								local istmt, tmp = stmt.fn:istmt()
								p:expr(istmt,istmt.iexpr)
								assert(p:take() == "]")
								path:index(tmp)
							end
						end
					end
					return
				end

				if p:peekNumber() then
					local n, s = p:number()
					stmt:number(queue,s)
					return
				end

				if p:peek() == '"' then
					p:take()
					local s = ""
					while p:peek() and not (p:peek() == '"') do
						if p:peek() == '\\' then
							s = s .. p:take()
						end
						s = s .. p:take()
					end
					assert(p:take() == '"')
					stmt:string(queue,s)
					return
				end

				assert(false, "expression expected " .. p:peekLine())
			end

			if not p:peekOperator() then
				step()
			end

			while p:peekOperator() do
				step()
				step()
			end

			p:skip()
		end,

		run = function(p)
			assert(p:take(3) == "lib", "expected library name")
			p.library = library(p:name())

			while p:peek() do
				(function()
					if p:peek() and p:peek(3) == "use" then
						p:take(3)
						p.library:link(p:name())
						return
					end
					if p:peek() and p:peek(8) == "#include" then
						p.library:Cinclude(p:takeLine())
						p:skip()
						return
					end
					if p:peek() and p:peek(6) == "struct" then
						p:struct()
						return
					end
					if p:peek() and p:peek(6) == "opaque" then
						p:opaque()
						return
					end
					if p:peek(2) == "{{" then
						p:take(2)
						local rawC = ""
						while p:peek() and not (p:peek(2) == "}}") do
							rawC = rawC .. p:take()
						end
						assert(p:take(2) == "}}", p:peekLine())
						p.library:C(rawC)
						p:skip()
						return
					end
					if p:peek() and p:peekLine():match("^${typePattern}%s+${namePattern}%s*=" % p) then
						p:init()
						return
					end
					if p:peek() and p:peekLine():match("^void%s+init%s*[(]" % p) then
						p:init2()
						return
					end
					if p:peek() and p:peekLine():match("^${typePattern}%s+${namePattern}%s*[(]" % p) then
						p:func()
						return
					end

					assert(false, "what? "..p:peekLine())
				end)()
			end
			return p
		end,
	}})
end

local function parse(file)
	newParser(file):run()
end

for i,a in ipairs(arg) do
	print(a)
	parse(a)
end

local common = io.open("build/common.h", "w")
common:write([[
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <threads.h>

#define notef(...) { fprintf(stderr, __VA_ARGS__); fputc('\n', stderr); }
#define fatalf(...) { notef(__VA_ARGS__); exit(EXIT_FAILURE); }
#define ensuref(cond,...) if (!(cond)) { notef(__VA_ARGS__); exit(EXIT_FAILURE); }

void* allot(size_t);
int main_main();
void rt_region_start();
void rt_region_end();
#define nop()

typedef char* string;

#define isLocal 1<<0
#define isTemp 1<<1
#define isPool 1<<2

struct interface {
	void* _concrete;
};

]])
common:close()

local init = io.open("build/_init.c", "w")

init:write("\n")
for n,lib in pairs(libraries) do
	init:write("void ${name}_init();\n" % lib)
end
init:write("int main_main();\n" % lib)
init:write("\n")

init:write("int main() {\n")
init:write("\trt_init();\n" % lib)
if libraries.io then
	init:write("\tio_init();\n" % lib)
end
if libraries.str then
	init:write("\tstr_init();\n" % lib)
end
for n,lib in pairs(libraries) do
	if n ~= "rt" and n ~= "io" and n ~= "str" then
		init:write("\t${name}_init();\n" % lib)
	end
end
init:write("\treturn main_main();\n")
init:write("}\n")
init:close()




for name,lib in pairs(libraries) do
	lib:prepare()
end

for name,lib in pairs(libraries) do
	lib:resolve()
end

for name,lib in pairs(libraries) do
	lib:embed()
end

for name,lib in pairs(libraries) do
	lib:output()
end