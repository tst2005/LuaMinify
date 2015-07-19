local assert = assert
local require = assert(require)
local error = assert(error)
local print = assert(print)
local table, string = require"table", require"string"
local ipairs = assert(ipairs)
--local pairs = assert(pairs)


require'strict'
--require'parselua'
local util = require'util'
assert( util.PrintTable )
assert( util.splitLines )
assert( util.lookupify )

local function debug_printf(...)
--	util.printf(...)
end

local print = print
local function debug_res(from, res)
	if res then
		if type(res) == "string" then
			print(from .. " result: '" .. res:gsub("\n","\\n") .. "'", type(res)..#res)
		elseif type(res) == "table" then
			print(from .. " t_result:")
			util.printf(res)
			print("/"..from)
		else
			print(from .. " result:", tostring(res), type(res))
		end
	end
end

--
-- FormatIdentity.lua
--
-- Returns the exact source code that was used to create an AST, preserving all
-- comments and whitespace.
-- This can be used to get back a Lua source after renaming some variables in
-- an AST.
--

-- rope <- out_appendStr <- [es]_appendStr <-

local rope = {} -- List of strings
--useless--local char = 1 -- was out.char
--useless--local line = 1 -- was out.line
--useless--local function lines_and_char_update(str) -- etait utilisé dans out_appendStr() apres l'ajout dans rope
--useless--	local lines = util.splitLines(str)
--useless--	if #lines == 1 then
--useless--		char = char + #str
--useless--	else
--useless--		line = line + #lines - 1
--useless--		local lastLine = lines[#lines]
--useless--		char = #lastLine
--useless--	end
--useless--end
--[[ -- etait dans out_appendToken()
	local str = token.Data
	local lines = util.splitLines(str)
	while line + #lines < token.Line do
		print("Inserting extra line")
		out.str  = out.str .. '\n'
		line = line + 1
		char = 1
	end
]]--


local tok_e_it = 1
local tok_s_it = 1

local formatExpr, formatStatement

local formatStatlist = function(statList)
	for _, stat in ipairs(statList.Body) do
		formatStatement(stat)
	end
end

local out_appendWhite -- cross use

local out_appendStr = function(str)
	table.insert(rope, str)
	return str
end

local out_appendToken = function(token)
	local res = out_appendWhite(token)
	debug_res("[out_appendToken]out_appendWhite", res)
	res = out_appendStr(token.Data)
	debug_res("[out_appendToken]out_appendStr", res)
	return res
end

local out_appendTokens = function(tokens)
	for _,token in ipairs(tokens) do
		out_appendToken( token )
	end
end

out_appendWhite = function(token)
	if token.LeadingWhite then
		return out_appendTokens( token.LeadingWhite )
		--out.str = out.str .. ' '
	end
end

local fe = {} -- format expression
local fs = {} -- format statement

--##############################################################################
--########## fe ################################################################

local function e_appendNextToken(str, expr)
print("la taille de expr.Tokens est ".. #expr.Tokens)
			local tok = expr.Tokens[tok_e_it];
			if str and tok.Data ~= str then
				error("Expected token '" .. str .. "'. Tokens: " .. util.PrintTable(expr.Tokens))
			end
local r	=		out_appendToken( tok )
--			tok_e_it = tok_e_it + 1
return r
end
local function e_appendToken(token)
local r =		out_appendToken( token )
--			tok_e_it = tok_e_it + 1
return r
end
local function e_appendWhite(expr)
			local tok = expr.Tokens[tok_e_it];
			if not tok then error("not tok (tok_e_it="..tok_e_it..") "..util.PrintTable(expr)) end
			out_appendWhite( tok )
--			tok_e_it = tok_e_it + 1
end
local function e_appendStr(str, expr)
	e_appendWhite(expr)
	return out_appendStr(str)
end
local function e_appendComma(mandatory, seperators, expr)
	local function e_peek()
		if tok_e_it < #expr.Tokens then
			return expr.Tokens[tok_e_it].Data
		end
	end
--	if true then
	seperators = seperators or { "," }
	seperators = util.lookupify( seperators )
	if not mandatory and not seperators[e_peek()] then
		return
	end
	assert(seperators[e_peek()], "Missing comma or semicolon")
	e_appendNextToken(nil, expr)
--	else
--		local p = e_peek()
--		if p == "," or p == ";" then
--			e_appendNextToken(nil, expr)
--		end
--	end
end

fe.VarExpr = function(expr)
	if expr.Variable then
		return e_appendStr( expr.Variable.Name, expr)
	else
		return e_appendStr( expr.Name, expr)
	end
end
fe.NumberExpr = function(expr)
	return e_appendToken( expr.Value )
end
fe.StringExpr = function(expr)
	return e_appendToken( expr.Value )
end
fe.BooleanExpr = function(expr)
	return e_appendNextToken( expr.Value and "true" or "false", expr )
end
fe.NilExpr = function(expr)
	return e_appendNextToken( "nil", expr )
end
fe.BinopExpr = function(expr)
			formatExpr(expr.Lhs)
			e_appendStr( expr.Op, expr)
			formatExpr(expr.Rhs)
end
fe.UnopExpr = function(expr)
			e_appendStr( expr.Op, expr)
			formatExpr(expr.Rhs)
end
fe.DotsExpr = function(expr)
	return e_appendNextToken( "...", expr )
end
fe.CallExpr = function(expr)
			formatExpr(expr.Base)
			e_appendNextToken( "(", expr )
			for i,arg in ipairs( expr.Arguments ) do
				formatExpr(arg)
				e_appendComma(i ~= #expr.Arguments, expr, expr)
			end
			e_appendNextToken( ")", expr )
end
fe.TableCallExpr = function(expr)
			formatExpr( expr.Base )
			formatExpr( expr.Arguments[1] )
end
fe.StringCallExpr = function(expr)
			formatExpr( expr.Base )
			e_appendToken( expr.Arguments[1] )
end
fe.IndexExpr = function(expr)
			formatExpr(expr.Base)
			e_appendNextToken( "[", expr )
			formatExpr(expr.Index)
			e_appendNextToken( "]", expr )
end
fe.MemberExpr = function(expr)
			formatExpr(expr.Base)
			e_appendNextToken(nil, expr)  -- . or :
			e_appendToken(expr.Ident)
end
fe.Function = function(expr)
			-- anonymous function
			e_appendNextToken( "function", expr )
			e_appendNextToken( "(", expr)
			if #expr.Arguments > 0 then
				for i = 1, #expr.Arguments do
					e_appendStr( expr.Arguments[i].Name, expr)
					if i ~= #expr.Arguments then
						e_appendNextToken(",", expr)
					elseif expr.VarArg then
						e_appendNextToken(",", expr)
						e_appendNextToken("...", expr)
					end
				end
			elseif expr.VarArg then
				e_appendNextToken("...", expr)
			end
			e_appendNextToken(")", expr)
			formatStatlist(expr.Body)
			e_appendNextToken("end", expr)
end
fe.ConstructorExpr = function(expr)
			e_appendNextToken( "{", expr )
			for i = 1, #expr.EntryList do
				local entry = expr.EntryList[i]
				if entry.Type == 'Key' then
					e_appendNextToken( "[", expr )
					formatExpr(entry.Key)
					e_appendNextToken( "]", expr )
					e_appendNextToken( "=", expr )
					formatExpr(entry.Value)
				elseif entry.Type == 'Value' then
					formatExpr(entry.Value)
				elseif entry.Type == 'KeyString' then
					e_appendStr(entry.Key, expr)
					e_appendNextToken( "=", expr )
					formatExpr(entry.Value)
				end
				e_appendComma( i ~= #expr.EntryList, { ",", ";" }, expr )
			end
			e_appendNextToken( "}", expr )
end
fe.Parentheses = function(expr)
			e_appendNextToken( "(", expr )
			formatExpr(expr.Inner)
			e_appendNextToken( ")", expr )
end

--##############################################################################
--########## fs ################################################################
local function s_appendNextToken(str, statement)
			local tok = statement.Tokens[tok_s_it];
			assert(tok, string.format("Not enough tokens for %q. First token at %i:%i",
				str, statement.Tokens[1].Line, statement.Tokens[1].Char))
			assert(tok.Data == str,
				string.format('Expected token %q, got %q', str, tok.Data))
local r =		out_appendToken( tok )
			tok_s_it = tok_s_it + 1
return r
end
--local function s_appendToken(token)
--			out_appendToken( str ) -- FIXME: str or token ?? s_appendToken was never used, it should be buggy
--			tok_s_it = tok_s_it + 1
--end
local function s_appendWhite(statement)
			local tok = statement.Tokens[tok_s_it];
local r =		out_appendWhite( tok )
			tok_s_it = tok_s_it + 1
return r
end
local function s_appendStr(str, statement)
	s_appendWhite(statement)
	return out_appendStr(str)
end
local function s_appendComma(mandatory, statement)
			if mandatory
			   or (tok_s_it < #statement.Tokens and statement.Tokens[tok_s_it].Data == ",") then
			   s_appendNextToken( ",", statement )
			end
end

fs.AssignmentStatement = function(statement)
			for i,v in ipairs(statement.Lhs) do
				formatExpr(v)
				s_appendComma( i ~= #statement.Lhs, statement )
			end
			if #statement.Rhs > 0 then
				s_appendNextToken( "=", statement )
				for i,v in ipairs(statement.Rhs) do
					formatExpr(v)
					s_appendComma( i ~= #statement.Rhs, statement )
				end
			end
	return
end
fs.CallStatement = function(statement)
			formatExpr(statement.Expression)
end
fs.LocalStatement = function(statement)
			s_appendNextToken( "local", statement )
			for i = 1, #statement.LocalList do
				s_appendStr( statement.LocalList[i].Name, statement)
				s_appendComma( i ~= #statement.LocalList, statement )
			end
			if #statement.InitList > 0 then
				s_appendNextToken( "=", statement )
				for i = 1, #statement.InitList do
					formatExpr(statement.InitList[i])
					s_appendComma( i ~= #statement.InitList, statement )
				end
			end
end
fs.IfStatement = function(statement)
			s_appendNextToken( "if", statement )
			formatExpr( statement.Clauses[1].Condition )
			s_appendNextToken( "then", statement)
			formatStatlist( statement.Clauses[1].Body )
			for i = 2, #statement.Clauses do
				local st = statement.Clauses[i]
				if st.Condition then
					s_appendNextToken( "elseif", statement )
					formatExpr(st.Condition)
					s_appendNextToken( "then", statement )
				else
					s_appendNextToken( "else", statement )
				end
				formatStatlist(st.Body)
			end
			s_appendNextToken( "end", statement )
end
fs.WhileStatement = function(statement)
			s_appendNextToken( "while", statement )
			formatExpr(statement.Condition)
			s_appendNextToken( "do", statement )
			formatStatlist(statement.Body)
			s_appendNextToken( "end", statement )
end
fs.DoStatement = function(statement)
			s_appendNextToken( "do", statement )
			formatStatlist(statement.Body)
			s_appendNextToken( "end", statement )
end
fs.ReturnStatement = function(statement)
			s_appendNextToken( "return", statement )
			for i = 1, #statement.Arguments do
				formatExpr(statement.Arguments[i])
				s_appendComma( i ~= #statement.Arguments, statement )
			end
end
fs.BreakStatement = function(statement)
			s_appendNextToken( "break", statement )
end
fs.RepeatStatement = function(statement)
			s_appendNextToken( "repeat", statement )
			formatStatlist(statement.Body)
			s_appendNextToken( "until", statement )
			formatExpr(statement.Condition)
end
fs.Function = function(statement)
			--print(util.PrintTable(statement))

			if statement.IsLocal then
				s_appendNextToken( "local", statement)
			end
			s_appendNextToken( "function", statement )

			if statement.IsLocal then
				s_appendStr( statement.Name.Name, statement )
			else
				formatExpr(statement.Name)
			end

			s_appendNextToken( "(", statement )
			if #statement.Arguments > 0 then
				for i = 1, #statement.Arguments do
					s_appendStr( statement.Arguments[i].Name, statement )
					s_appendComma( i ~= #statement.Arguments or statement.VarArg, statement )
					if i == #statement.Arguments and statement.VarArg then
						s_appendNextToken( "...", statement )
					end
				end
			elseif statement.VarArg then
				s_appendNextToken( "...", statement )
			end
			s_appendNextToken( ")", statement )

			formatStatlist(statement.Body)
			s_appendNextToken( "end", statement )
end
fs.GenericForStatement = function(statement)
			s_appendNextToken( "for", statement )
			for i = 1, #statement.VariableList do
				s_appendStr( statement.VariableList[i].Name, statement)
				s_appendComma( i ~= #statement.VariableList, statement )
			end
			s_appendNextToken( "in", statement )
			for i = 1, #statement.Generators do
				formatExpr(statement.Generators[i])
				s_appendComma( i ~= #statement.Generators, statement )
			end
			s_appendNextToken( "do", statement )
			formatStatlist(statement.Body)
			s_appendNextToken( "end", statement )
end
fs.NumericForStatement = function(statement)
			s_appendNextToken( "for", statement )
			s_appendStr( statement.Variable.Name, statement)
			s_appendNextToken( "=", statement )
			formatExpr(statement.Start)
			s_appendNextToken( ",", statement )
			formatExpr(statement.End)
			if statement.Step then
				s_appendNextToken( ",", statement )
				formatExpr(statement.Step)
			end
			s_appendNextToken( "do", statement )
			formatStatlist(statement.Body)
			s_appendNextToken( "end", statement )
end
fs.LabelStatement = function(statement)
			s_appendNextToken( "::", statement )
			s_appendStr( statement.Label, statement)
			s_appendNextToken( "::", statement )
end
fs.GotoStatement = function(statement)
			s_appendNextToken( "goto", statement )
			s_appendStr( statement.Label, statement)
end
fs.Eof = function(statement)
			s_appendWhite(statement)
end


--##############################################################################

formatExpr = function(expr)

	tok_e_it = 1 -- reset
	debug_printf("formatExpr(%s) at line %i", expr.AstType, expr.Tokens[1] and expr.Tokens[1].Line or -1)

	local handler = fe[expr.AstType]
	local res
	if handler then
		res = handler(expr)
		if res then print("expr result", res) end
	else
		print("Unknown AST Type: ", expr.AstType)
	end

--	assert(tok_e_it == #expr.Tokens + 1, "formatExpr: tok_e_it == #expr.Tokens + 1")
	debug_printf("/formatExpr")
	return res
end


formatStatement = function(statement)
	tok_s_it = 1 -- reset

	debug_printf("")
	debug_printf(string.format("formatStatement(%s) at line %i", statement.AstType, statement.Tokens[1] and statement.Tokens[1].Line or -1))

	local handler = fs[statement.AstType]
	local res
	if handler then
		res = handler(statement)
		if res then print("stmt result", res) end
	else
		print("Unknown AST Type: ", statement.AstType)
	end

	if statement.Semicolon then
		s_appendNextToken(";", statement)
	end

	assert(tok_s_it == #statement.Tokens + 1)
	debug_printf("/formatStatment")
	return res
end


local function Format(ast)
	rope = {} -- List of strings
--	line = 1
--	char = 1
	formatStatlist(ast)
	return table.concat(rope)
end

return Format
