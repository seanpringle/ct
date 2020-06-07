getmetatable("").__mod = function(s, tab)
	return (s:gsub('($%b{})', function(w) return tostring(tab[w:sub(3, -2)]) or tostring(w) end))
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

local builtinTypes = {
	int = true,
	bool = true,
	double = true,
	void = true,
	string = true,
}

local symbolMT = {
	__tostring = function(sym)
		return sym.lib and (sym.lib.name .."_".. sym.name) or sym.name
	end,

	__index = {
		resolve = function(sym)
			if builtinTypes[sym.name] then
				return
			end
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
			local sym = st.lib:symbolref(type)
			local field = setmetatable({
				sym = sym,
				name = name,
				cells = cells,
			}, fieldMT)
			st.fields[name] = field
			st.order[#st.order+1] = name
			if type == "embed" then
				field.embed = true
				field.esym = st.lib:symbolref(name)
			end
		end,

		embed = function(st)
			assert(not st.embedding)
			st.embedding = true
			local rfields = {}
			local efields = {}
			for n,f in pairs(st.fields) do
				if f.embed then
					assert(type(f.esym.lib) == "table")
					assert(type(f.esym.lib.structs) == "table")
					assert(type(f.esym.lib.structs[f.name]) == "table")
					local sst = f.esym.lib.structs[f.name]
					for sn,sf in pairs(sst.fields) do
						--print("embed", n, sn)
						assert(not st.fields[sn], "duplicate embedded field "..sn)
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
				--print("WOIWOW", f.name)
			end
			st.embedding = false
		end,

		declareConcrete = function(st)
			return "struct _${name}_concrete;\n" % { name = st.sym }
				.. "typedef struct _${name}_concrete ${name}_concrete;\n" % { name = st.sym }
		end,

		declare2Concrete = function(st)
			local fields = ""
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
			local fields = ""
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
			out[#out+1] = "\t(${sym}) {" % st
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
			return "${sym} ${sym}_pool();\n" % st
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
			out[#out+1] = "\treturn (${sym}) {" % st
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
			out[#out+1] = "${sym} ${sym}_pool() {" % st
			out[#out+1] = "\t${sym}_concrete *_p = calloc(1, sizeof(${sym}_concrete));" % st
			out[#out+1] = "\treturn (${sym}) {" % st
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

local exprCallMT = {
	__tostring = function(e)
		return e.name
	end,

	__index = {
		define = function(e)
			assert(type(e.sym.lib) == "table")
			assert(type(e.sym.lib.funcs[e.sym.name]) == "table")
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
				if #oexpr > 0 and #iexpr > 0 then
					return "${keyword} (${iexpr}) {\n\t${oexpr};\n\t}" % {
						keyword = sm.keyword,
						oexpr = table.concat(oexpr),
						iexpr = table.concat(iexpr),
					}
				end
				if #oexpr > 0 then
					return "${keyword} {\n\t${oexpr};\n\t}" % {
						keyword = sm.keyword,
						oexpr = table.concat(oexpr),
					}
				end
				return "${keyword} ${iexpr}" % {
					keyword = sm.keyword,
					iexpr = table.concat(iexpr),
				}
			end

			if #oexpr > 0 then
				return "${oexpr} = ${iexpr}" % {
					oexpr = table.concat(oexpr),
					iexpr = table.concat(iexpr),
				}
			end

			if #iexpr > 0 then
				return "${iexpr}" % {
					iexpr = table.concat(iexpr),
				}
			end

			return "// "..#oexpr.." "..#iexpr
		end,
	},
}

local varMT = {
	__tostring = function(va)
		return va.sym
	end,

	__index = {

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
				term = true,
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
				term = true,
			}, stmtMT)
			local tmp = "_index"..#fn.stmts
			table.insert(fn.stmts, #fn.stmts, stmt)
			fn:var("int", tmp)
			stmt:reference(stmt.oexpr,tmp)
			return stmt, tmp
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
				stmts[#stmts+1] = "\t"..s:define()..(s.term and ";" or "").."\n"
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

		use = function(lib,name)
			lib.using[name] = true
		end,

		C = function(lib,rawC)
			lib.Cchunks[#lib.Cchunks+1] = rawC
		end,

		symbol = function(lib, name)
			assert(not name:match(":"), "symbol definition outside library "..name)
			assert(not lib.symbols[name], "duplicate symbol "..name)
			assert(not builtinTypes[name], "symbol is a built-in type "..name)
			local sym = setmetatable({
				lib = lib.name,
				name = name,
			}, symbolMT)
			lib.symbols[name] = sym
			return sym
		end,

		symbolref = function(lib, name)
			local lname = lib.name

			if builtinTypes[name] then
				assert(not name:match(":"), "symbol is a built-in type "..name)
				lname = nil
			end

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
			}, structMT)
			lib.structs[name] = st
			return st
		end,

		opaque = function(lib, ctype, name)
			local sym = lib:symbol(name)
			local op = setmetatable({
				lib = lib,
				sym = sym,
				ctype = ctype,
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
		end,

		embed = function(lib)
			for i,st in pairs(lib.structs) do
				st:embed()
			end
		end,

		output = function(lib)
			--print("output", lib)
			local h = io.open("build/"..lib.name..".h", "w+")
			local c = io.open("build/"..lib.name..".c", "w+")

			c:write("#include \"common.h\"\n")
			c:write("#include \"${name}.h\"\n" % lib)

			for name,b in pairs(lib.using) do
				c:write("#include \"${name}.h\"\n" % { name = name })
			end

			c:write("\n")

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

			for i,obj in pairs(lib.funcs) do
				h:write(obj:declare().."\n")
			end

			for i,obj in pairs(lib.Cchunks) do
				c:write(obj.."\n")
			end

			for i,obj in pairs(lib.funcs) do
				c:write(obj:define().."\n")
			end

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
		using = {},
		Cchunks = {},
	}, libMT)
	libraries[lib] = l
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
			["is"] = "is",
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
				if p.queue[i] == "\n" then
					break
				end
				s = (s or "") .. p.queue[i]
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
			assert(s, "expected word")
			return s
		end,

		peekName = function(p)
			p:skip()
			return p:peek() and p:peek():match("[a-zA-Z]")
		end,

		name = function(p)
			assert(p:peekName(), "expected name")
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
					return true, k
				end
			end
		end,

		operator = function(p)
			local ok, op = p:peekOperator(p)
			if ok then
				p:take(op:len())
			end
			return op
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
			--print("struct", p:peekLine())
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
			--print("opaque", p:peekLine())
			assert(p:name() == "opaque", "expected opaque")
			local ctype = p:word()
			local oname = p:name()
			p.library:opaque(ctype, oname)
		end,

		func = function(p)
			p:skip()
			--print("func", p:peekLine())
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
			--print("block", p:peekLine())
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
			assert(p:take() == "{", "expected start of block")
			p:skip()
			while p:peek() and not (p:peek() == "}") do
				p:stmt(func)
			end
			assert(p:take() == "}")
			p:skip()
		end,

		stmt = function(p,func)
			p:skip()
			--print("stmt", p:peekLine())
			local stmt = func:stmt()

			if p:peekLine():match("^return ") then
				p:take(6)
				stmt.keyword = "return"
				p:expr(stmt,stmt.iexpr)
				return
			end

			if p:peekLine():match("^if ") then
				p:take(2)
				stmt.keyword = "if"
				stmt.term = false
				p:expr(stmt,stmt.iexpr)
				local start = #func.stmts
				p:block(func)
				while start < #func.stmts do
					stmt.oexpr[#stmt.oexpr+1] = table.remove(func.stmts,start+1)
				end
				return
			end

			if p:peekLine():match("^else ") then
				p:take(4)
				p:skip()
				if p:peekLine():match("^if") then
					p:take(2)
					stmt.keyword = "else if"
					stmt.term = false
					p:expr(stmt,stmt.iexpr)
					local start = #func.stmts
					p:block(func)
					while start < #func.stmts do
						stmt.oexpr[#stmt.oexpr+1] = table.remove(func.stmts,start+1)
					end
				else
					stmt.keyword = "else"
					stmt.term = false
					--p:expr(stmt,stmt.iexpr)
					local start = #func.stmts
					p:block(func)
					while start < #func.stmts do
						stmt.oexpr[#stmt.oexpr+1] = table.remove(func.stmts,start+1)
					end
				end
				return
			end

			if p:peekLine():match("^while ") then
				p:take(5)
				stmt.keyword = "while"
				stmt.term = false
				p:expr(stmt,stmt.iexpr)
				local start = #func.stmts
				p:block(func)
				while start < #func.stmts do
					stmt.oexpr[#stmt.oexpr+1] = table.remove(func.stmts,start+1)
				end
				return
			end

			if p:peekLine():match("^${tp}%s+${np}%s*=" % { tp = p.typePattern, np = p.namePattern }) then
				local vtype = p:name()
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
			--print("expr", p:peekLine())

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
							p:expr(stmt,args)
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

				assert(false, "expression expected")
			end

			step()

			while p:peekOperator() do
				step()
				step()
			end

			p:skip()
		end,

		run = function(p)
			assert(p:take(3) == "lib")
			p.library = library(p:name())

			while p:peek() do
				if p:peek() and p:peek(3) == "use" then
					p:take(3)
					p.library:use(p:name())
				end
				if p:peek() and p:peek(6) == "struct" then
					p:struct()
				end
				if p:peek() and p:peek(6) == "opaque" then
					p:opaque()
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
				end
				if p:peek() and p:peekLine():match("^${typePattern}%s+${namePattern}%s*[(]" % p) then
					p:func()
				end
			end
			return p
		end,
	}})
end

local function parse(file)
	newParser(file):run()
end

parse("main.ct")
parse("rt.ct")
parse("io.ct")
parse("str.ct")

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

typedef char* string;
]])
common:close()

local build = io.open("build/build.sh", "w")
build:write("#!/bin/sh\n")
for name,lib in pairs(libraries) do
	build:write("cc -O1 -g -Wall -Werror -std=c11 -c ${name}.c\n" % lib)
end
build:write("cc -O1 -g -Wall -Werror -std=c11 -flto -o main *.o\n" % lib)
build:close()
os.execute("chmod 755 build/build.sh")

for name,lib in pairs(libraries) do
	lib:resolve()
end

for name,lib in pairs(libraries) do
	lib:embed()
end

for name,lib in pairs(libraries) do
	lib:output()
end