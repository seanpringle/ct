getmetatable("").__mod = function(s, tab)
	return (s:gsub('($%b{})', function(w) return tab[w:sub(3, -2)] or w end))
end

local function slurp(path)
	local file = io.open(path, "rb")
	if not file then return nil end
	local content = file:read("*a")
	file:close()
	return content
end

local source = nil
local output = nil
local buffers = {}
local blocks = {}
local sequence = 1

local library = nil
local using = {}

local headers = {
	includes = {},
	structdecs = {},
	structs = {},
	functions = {},
}

local types = {
	auto = {
		itype = "",
	},
	void = {
		itype = "void",
	},
	bool = {
		itype = "bool",
	},
	int = {
		itype = "int",
	},
	uint = {
		itype = "uint",
	},
	float = {
		itype = "float",
	},
	double = {
		itype = "double",
	},
	int32 = {
		itype = "int32_t",
	},
	int64 = {
		itype = "int64_t",
	},
	uint32 = {
		itype = "uint32_t",
	},
	uint64 = {
		itype = "uint64_t",
	},
	float32 = {
		itype = "float",
	},
	float64 = {
		itype = "double",
	},
	string = {
		itype = "char*",
	},
}

local symbols = {
	["{"] = true,
	["}"] = true,
	[":"] = true,
	["+"] = true,
	["-"] = true,
	["*"] = true,
	["/"] = true,
	["%"] = true,
	["="] = true,
	["<"] = true,
	[">"] = true,
	["&"] = true,
	["|"] = true,
	["~"] = true,
}

local operator = {
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
}

local function subsetOf(f, t)
	local ok = true
	for i,ff in ipairs(f.fields) do
		local found = false
		for j,tf in ipairs(t.fields) do
			if ff.type == tf.type and ff.name == tf.name then
				found = true
				break
			end
		end
		if not found then
			ok = false
			break
		end
	end
	return ok
end

local function supersetOf(f, t)
	return subsetOf(t, f)
end

local function out(s)
	if #buffers > 0 then
		buffers[#buffers] = buffers[#buffers] .. s
	else
		output:write(s)
	end
end

local function capture()
	buffers[#buffers+1] = ""
end

local function retrieve()
	local s = buffers[#buffers]
	buffers[#buffers] = nil
	return s
end

local function skip()
	source = source:gsub("^%s*", "")
end

local function word()
	skip()
	local w = source:gmatch("[^%s]+")()
	source = source:sub(w:len()+1)
	skip()
	return w
end

local function char()
	skip()
	local w = source:sub(1,1)
	source = source:sub(2)
	skip()
	return w
end

local function wordName()
	skip()
	local w = source:gmatch("[%w_]+")()
	source = source:sub(w:len()+1)
	skip()
	return w
end

local function peekName()
	skip()
	local w = source:sub(1,1):gmatch("[%w_]+")()
	return w ~= nil
end

local function wordNumber()
	skip()
	local w = source:gmatch("[%d]+")()
	source = source:sub(w:len()+1)
	skip()
	return w
end

local function peekNumber()
	skip()
	return source:sub(1,1):match("%d")
end

local function wordIndex()
	skip()
	local w = source:gmatch("[^]]+")()
	source = source:sub(w:len()+1)
	skip()
	return w
end

local function prefix(str, start)
	return str:sub(1, #start) == start
end

local function peek(w)
	skip()
	return prefix(source, w)
end

local function istype(t)
	return types[t] ~= nil
end

local function wordType(lib)
	local explicit = false
	local wlib = lib
	local wtype = wordName()
	if peek(":") then
		explicit = true
		char()
		wlib = wtype
		wtype = wordName()
	end
	if not explicit and types[wtype] ~= nil then
		return wtype, wtype
	end
	for i,l in ipairs(using) do
		if l == wlib then
			return wlib.."_"..wtype, wtype
		end
	end
	if wlib == lib then
		return wlib.."_"..wtype, wtype
	end
	assert(false, "unknown library ${lib}" % { lib = wlib })
end

local functions = {
}

local function peekOperator()
	for op,f in pairs(operator) do
		if peek(op) then
			return true
		end
	end
	return false
end

local function wordOperator()
	for op,f in pairs(operator) do
		if peek(op) then
			local w = source:sub(1,op:len())
			source = source:sub(w:len()+1)
			return w
		end
	end
	assert(false)
end

local function indent(level)
	for i = 1,level do
		out("\t")
	end
end

local expr = nil
local stmt = nil
local block = nil

local greedy = function()
	while peekOperator() do
		local op = wordOperator()
		if op == "is" then
			local etype = wordType(library)
			out("._concrete_type == type_${etype}" % { etype = etype })
		else
			out(operator[op])
			expr(0)
		end
	end
end

local comments = function()
	skip()
	while prefix(source, "/*") or prefix(source, "//") do
		if prefix(source, "/*") then
			while source:len() > 0 and not prefix(source, "*/") do
				out(source:sub(1,1))
				source = source:sub(2)
			end
			if prefix(source, "*/") then
				source = source:sub(3)
			end
			out("*/")
		end
		if prefix(source, "//") then
			while source:len() > 0 and not prefix(source, "\n") do
				out(source:sub(1,1))
				source = source:sub(2)
			end
		end
		while source:match("^%s") do
			out(source:sub(1,1))
			source = source:sub(2)
		end
	end
end

expr = function(level)
	comments()
	local scope = blocks[#blocks]

	local location = "local"
	if peek("local") then
		word()
		location = "local"
	end
	if peek("temp") then
		word()
		location = "temp"
	end
	if peek("pool") then
		word()
		location = "pool"
	end

	if peek("cast") then
		wordName()
		local src = wordName()
		local etype, ename = wordType(library)
		local dst = wordName()
		scope.declare[#scope.declare+1] = "${ctype} ${dst};\n" % { ctype = types[etype].itype, dst = dst }
		out("(${src}._concrete_type == type_${etype} && ({ ${dst} = recast_${etype}((struct interface*)&${src}); true; }))"
			% { src = src, dst = dst, ename = ename, etype = etype, itype = types[etype].itype })
		return
	end

	if peek("{") then
		char()
		while not peek("}") do
			local fieldName = wordName()
			assert(char() == "=", source)
			out("*_i.${name} = " % { name = fieldName })
			expr(0)
			if peek(",") then
				char()
			end
			out("; ")
		end
		char()
		return
	end

	if peek("(") then
		out(char())
		expr(0)
		assert(peek(")"))
		out(char())
		greedy()
		return
	end

	if peek('"') then
		out(char())
		while not prefix(source,'"') do
			if prefix(source, '\\') then
				out(source:sub(1,1))
				source = source:sub(2)
			end
			out(source:sub(1,1))
			source = source:sub(2)
		end
		assert(char() == '"')
		out('"')
		greedy()
		return
	end

	if peekNumber() then
		out(wordNumber())
		greedy()
		return
	end

	if peek("--") then
		char() char()
		out("--")
		out(wordName())
		greedy()
		return
	end

	if peek("++") then
		char() char()
		out("++")
		out(wordName())
		greedy()
		return
	end

	local etype, ename = wordType(library)

	if peek(".") then

		local path = "${name}" % { name = ename }
		local i = 0
		while peek(".") do
			char()
			local field = wordName()
			local sep = i%2==0 and "." or "->"
			if peek("[") then
				char()
				local index = wordIndex()
				assert(char() == "]")
				--field = "${field}+${index}" % { field = field, index = index }
				field = "${field}+bounds(${path}${sep}_${field}_len, ${index})"
					% { path = path, sep = sep, field = field, index = index }
			end
			path = path .. "${sep}${field}" % { sep = sep, field = field }
			i = i+1
		end
		out("*(${path})" % { path = path })

		if peek("=") and not peek("==") then
			char()
			out(" = ")
			expr(0)
		else
			greedy()
		end
		return
	end

	if peek("(") then
		assert(functions[etype] ~= nil, "unknown function ${etype} ${ename}" % { etype = etype, ename = ename })
		char()
		out("${name}(" % { name = etype })
		for i,a in ipairs(functions[etype].args) do
			assert(not peek(")"))
			--print(etype)
			if types[a.atype].cast == nil then
				expr(0)
			else
				out("${cast}(" % types[a.atype])
				expr(0)
				out(")")
			end
			if not peek(")") then
				out(", ")
			end
			if peek(",") then
				char()
			end
		end
		if functions[etype].variadic then
			while not peek(")") do
				expr(0)
				if peek(",") then
					char()
				end
				if not peek(")") then
					out(", ")
				end
			end
		end
		assert(char() == ")")
		out(")")
		greedy()
		return
	end

	if istype(etype) then

		out("${type} " % { type = types[etype].itype })

		while peekName() do
			local ename = wordName()

			out("${name}" % { name = ename })

			local assign = peek("=") and not peek("==")

			if assign then
				char()
			end

			local interface = types[etype].cast ~= nil and peek("{")

			scope.vars[ename] = {
				type = etype,
			}

			if location == "local" and interface then
				local cname = "_l${n}" % { n = #scope.declare+1 }
				scope.vars[ename].lname = cname
				scope.declare[#scope.declare+1] = "${type} ${cname};\n" % { type = types[etype].ctype, cname = cname }

				out(" = ({ ${itype} _i = local_${etype}(&${cname}); " % { itype = types[etype].itype, etype = etype, cname = cname })
				if assign then
					expr(0)
				end
				out("_i; })")
			end

			if location ~= "local" and interface then
				out(" = ({ ${itype} _i = ${location}_${name}(); " % { location = location, itype = types[etype].itype, name = etype })
				if assign then
					expr(0)
				end
				out("_i; })")
			end

			if not interface then
--				assert(location == "local")
				if assign then
					out(" = ")
					expr(0)
				end
			end

			if not peek(",") then
				break
			end

			out(char())
			out(" ")
		end
		return
	end

	if not istype(etype) and scope.vars[ename] ~= nil and peek("=") and not peek("==") then
		etype = scope.vars[ename].type

		out("${name}" % { name = ename })

		local assign = true

		if assign then
			char()
		end

		local interface = types[etype].cast ~= nil and peek("{")

		if location == "local" and interface then
			local cname = scope.vars[ename].lname
			out(" = ({ ${itype} _i = local_${etype}(&${cname}); " % { itype = types[etype].itype, etype = etype, cname = cname })
			if assign then
				expr(0)
			end
			out("_i; })")
		end

		if location ~= "local" and interface then
			out(" = ({ ${itype} _i = ${location}_${name}(); " % { location = location, itype = types[etype].itype, name = etype })
			if assign then
				expr(0)
			end
			out("_i; })")
		end

		if not interface then
--			assert(location == "local")
			if assign then
				out(" = ")
				expr(0)
			end
		end
		return
	end

	if peek("--") then
		char() char()
		out(ename)
		out("--")
		greedy()
		return
	end

	if peek("++") then
		char() char()
		out(ename)
		out("++")
		greedy()
		return
	end

	out(ename)
	greedy()
end

stmt = function(level)
	comments()

	if peek("{{") then
		source = source:sub(3)
		if level > 0 then
			indent(level)
			out("{")
			indent(level)
		end
		while not prefix(source, "}}") do
			local c = source:sub(1,1)
			source = source:sub(2)
			out(c)
		end
		assert(prefix(source, "}}"))
		source = source:sub(3)
		if level > 0 then
			out("}\n")
		end
		return
	end

	if peek("return") then
		word()
		indent(level)
		out("return ")
		expr(0)
		out(";\n")
		return
	end

	if peek("if") then
		wordName()
		indent(level)
		out("if (")
		expr(0)
		out(")")
		if peek("{") then
			block(level)
			if peek("else") then
				indent(level)
				out(wordName())
				if peek("{") then
					block(level)
				else
					expr(0)
					out(";")
				end
			end
		else
			expr(0)
			out(";")
		end
		out("\n")
		return
	end

	if peek("while") then
		wordName()
		indent(level)
		out("while (")
		expr(0)
		out(")")
		if peek("{") then
			block(level)
		else
			expr(0)
			out(";")
		end
		out("\n")
		return
	end

	if peek("for") then
		wordName()
		indent(level)
		out("for (")
		expr(0)
		assert(char() == ";")
		out("; ")
		expr(0)
		assert(char() == ";")
		out("; ")
		expr(0)
		out(")")
		if peek("{") then
			block(level)
		else
			expr(0)
			out(";")
		end
		out("\n")
		return
	end

	indent(level)
	expr(level)
	out(";\n")
end

block = function(level)
	comments()

	skip()
	assert(prefix(source, "{"), "expected block")
	if peek("{{") then
		out("{")
		stmt(0)
		out("}\n")
	else
		local outer = blocks[#blocks]
		local inner = {
			declare = {},
			vars = {},
		}

		blocks[#blocks+1] = inner

		for k,v in pairs(outer.vars) do
			inner.vars[k] = v
		end

		char()
		capture()
		while not peek("}") do
			stmt(level+1)
		end
		assert(char() == "}")
		local body = retrieve()
		out("{\n")
		for i,d in ipairs(inner.declare) do
			indent(level+1)
			out(d)
		end
		out(body)
		indent(level)
		out("}")
		if level == 0 then
			out("\n")
		end

		blocks[#blocks] = nil
	end
end

local function define()
	comments()

	if peek("#include") then
		word()
		headers.includes[word()] = true
		return
	end

	if peek("lib") then
		word()
		library = wordName()
		return
	end

	if peek("use") then
		word()
		using[#using+1] = wordName()
		return
	end

	if peek("{{") then
		stmt(0)
		return
	end

	if peek("opaque") then
		word()
		local itype = word()
		local otype = library .."_".. wordName()
		types[otype] = { itype = itype }
		return
	end

	if peek("struct") then
		word()
		local struct = library .. "_" .. wordName()
		assert(not istype(struct), "type ${name} already exists" % { name = struct })

		local id = sequence
		sequence = sequence+1

		local fields = {}
		assert(char() == "{")
		while not peek("}") do
			if peek("embed") then
				word()
				local embedType = wordType(library)
				assert(types[embedType] ~= nil, "unknown embedded type ${name}" % { name = embedType })
				for i,f in ipairs(types[embedType].fields) do
					fields[#fields+1] = f
				end
			else
				local fieldType = wordType(library)
				local fieldName = wordName()
				local arraySize = nil
				assert(types[fieldType] ~= nil, "unknown type ${type} for field ${field}" % { type = fieldType, name = fieldName })
				if peek("[") then
					char()
					arraySize = wordNumber()
					assert(char() == "]")
				end
				fields[#fields+1] = {
					type = types[fieldType].itype,
					name = fieldName,
					array = arraySize,
				}
			end
		end
		assert(char() == "}")

		capture()
			headers.structdecs[#headers.structdecs+1] = "struct concrete_${name};\n" % { name = struct }
			out("struct concrete_${name} {\n" % { name = struct })
			for i,f in ipairs(fields) do
				if f.array ~= nil then
					out("\t${type} ${name}[${array}];\n" % f)
				else
					out("\t${type} ${name};\n" % f)
				end
			end
			if #fields == 0 then
				out("\tchar _dummy;\n")
			end
			out("};\n\n")

			headers.structdecs[#headers.structdecs+1] = "struct interface_${name};\n" % { name = struct }
			out("struct interface_${name} {\n" % { name = struct })
			out("\tint _concrete_type;\n")
			out("\tvoid *_concrete_ptr;\n" % { name = struct })
			for i,f in ipairs(fields) do
				if f.array ~= nil then
					out("\tint _${name}_len;\n" % f)
				end
				out("\t${type} *${name};\n" % f)
			end
			out("};\n\n")

			out("#define type_${name} ${id}\n\n" % { name = struct, id = id })

			out("#define local_${name}(l) ({\\\n" % { name = struct })
			out("\tstruct concrete_${name} *c = (l);\\\n" % { name = struct })
			out("\tmemset(c, 0, sizeof(struct concrete_${name}));\\\n" % { name = struct })
			out("\t(struct interface_${name}){\\\n" % { name = struct })
			out("\t\t._concrete_type = ${id},\\\n" % { id = id })
			out("\t\t._concrete_ptr = c,\\\n" % { id = id })
			for i,f in ipairs(fields) do
				if f.array ~= nil then
					out("\t\t._${name}_len = sizeof(c->${name})/sizeof(c->${name}[0]),\\\n" % f)
					out("\t\t.${name} = c->${name},\\\n" % f)
				else
					out("\t\t.${name} = &c->${name},\\\n" % f)
				end
			end
			out("\t}; })\n\n")

			out("struct interface_${name} temp_${name}() {\n" % { name = struct })
			if #fields > 0 then
				out("\tstruct concrete_${name} *c = allot(sizeof(struct concrete_${name}));\n" % { name = struct })
			end
			out("\treturn (struct interface_${name}){\n" % { name = struct })
			out("\t\t._concrete_type = ${id},\n" % { id = id })
			if #fields > 0 then
				out("\t\t._concrete_ptr = c,\n" % { id = id })
			end
			for i,f in ipairs(fields) do
				if f.array ~= nil then
					out("\t\t._${name}_len = sizeof(c->${name})/sizeof(c->${name}[0]),\\\n" % f)
					out("\t\t.${name} = c->${name},\n" % f)
				else
					out("\t\t.${name} = &c->${name},\n" % f)
				end
			end
			out("\t};\n")
			out("};\n\n")

			out("struct interface_${name} pool_${name}() {\n" % { name = struct })
			if #fields > 0 then
				out("\tstruct concrete_${name} *c = calloc(1, sizeof(struct concrete_${name}));\n" % { name = struct })
			end
			out("\treturn (struct interface_${name}){\n" % { name = struct })
			out("\t\t._concrete_type = ${id},\n" % { id = id })
			if #fields > 0 then
				out("\t\t._concrete_ptr = c,\n" % { id = id })
			end
			for i,f in ipairs(fields) do
				if f.array ~= nil then
					out("\t\t._${name}_len = sizeof(c->${name})/sizeof(c->${name}[0]),\\\n" % f)
					out("\t\t.${name} = c->${name},\n" % f)
				else
					out("\t\t.${name} = &c->${name},\n" % f)
				end
			end
			out("\t};\n")
			out("};\n\n")

			out("#define cast_${name}(s) ({ __typeof__ (s) _s = (s);\\\n" % { name = struct })
			out("\t(struct interface_${name}){\\\n" % { name = struct })
			out("\t\t._concrete_type = _s._concrete_type,\\\n")
			out("\t\t._concrete_ptr = _s._concrete_ptr,\\\n")
			for i,f in ipairs(fields) do
				out("\t\t.${name} = _s.${name},\\\n" % f)
			end
			out("\t}; })\n\n")

		headers.structs[#headers.structs+1] = retrieve()

		types[struct] = {
			itype = "struct interface_${name}" % { name = struct },
			ctype = "struct concrete_${name}" % { name = struct },
			cast = "cast_${name}" % { name = struct },
			fields = fields,
			id = id,
		}
		return
	end

	local vtype = wordType(library)
	assert(types[vtype] ~= nil, "unknown type ${name}" % { name = vtype })

	local vname = library .."_".. wordName()

	if peek("(") then

		char()
		local args = {}
		local variadic = false
		while not peek(")") do

			if peek("...") then
				variadic = true
				char()
				char()
				char()
				break
			end

			local atype, aname = wordType(library)
			if types[atype] == nil then
				atype = args[#args].atype
			else
				aname = wordName()
			end

			assert(types[atype] ~= nil, "unknown argument type ${name}" % { name = vtype })

			args[#args+1] = {
				itype = types[atype].itype,
				atype = atype,
				name = aname,
			}
			if peek(",") then
				char()
			end
		end
		assert(char() == ")")

		functions[vname] = {
			variadic = variadic,
			args = args,
		}

		capture()
			out("${type} ${name}(" % { type = types[vtype].itype, name = vname })
			for i,a in ipairs(args) do
				out("${itype} ${name}" % a)
				if i < #args then
					out(", ")
				end
			end
			if variadic then
				out(", ...")
			end
			out(");\n\n")
		headers.functions[#headers.functions+1] = retrieve()

		out("${type} ${name}(" % { type = types[vtype].itype, name = vname })
		for i,a in ipairs(args) do
			out("${itype} ${name}" % a)
			if i < #args then
				out(", ")
			end
		end
		if variadic then
			out(", ...")
		end
		out(") ")
		assert(peek("{"), "expected function block")
		block(0)
		out("\n")
		return
	end
end

output = io.open("main.c", "w")

out([[
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

void oob(int i) {
	ensuref(false, "out of bounds: %d", i);
}

#define bounds(l,i) ({ int _l = (l); int _i = (i); if (_i < 0 || _i >= _l) oob(_i); _i; })

struct interface {
	int _concrete_type;
	void *_concrete_ptr;
};

]])

capture()

local parse = function(lib)

	blocks = {{
		declare = {},
		vars = {},
	}}

	library = lib
	source = slurp("${lib}.ct" % { lib = lib })

	while string.len(source) > 0 do
		(function()
			skip()
			if string.len(source) == 0 then
				return
			end
			define()
		end)()
	end
end

parse("rt")
parse("io")
parse("str")

for i,l in ipairs(arg) do
	parse(l)
end

local code = retrieve()

for k,v in pairs(headers.includes) do
	out("#include ${word}\n" % { word = k })
end

for i,v in ipairs(headers.structdecs) do
	out(v)
end

out("\n")

for i,v in ipairs(headers.structs) do
	out(v)
end

-- for each subset, find supersets
for sname,sub in pairs(types) do
	if sub.id ~= nil then
		local fmt = {
			sname = sname,
			itype = sub.itype,
			ctype = sub.ctype,
			id = sub.id,
		}
		out("${itype} recast_${sname}(struct interface *ii) {\n" % fmt)
		out("\tassert(ii->_concrete_type == type_${sname});\n" % fmt)
		if #sub.fields > 0 then
			out("\t${ctype} *ic = ii->_concrete_ptr;\n" % fmt)
		end
		out("\treturn (${itype}){\n" % fmt)
		for i,field in ipairs(sub.fields) do
			out("\t\t.${name} = &ic->${name},\n" % field)
		end
		out("\t};\n")
		out("}\n\n")
	end
end

for i,v in ipairs(headers.functions) do
	out(v)
end

out(code)

output:close()