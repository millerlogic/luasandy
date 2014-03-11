-- LuaSandy - sandbox for the Lua language.
-- Copyright 2012-2014 Christopher E. Miller
-- License: GPLv3, see LICENSE file.


function _null_print()
end


function _sandboxhook(a, b, func)
	if not func or not func() then
		debug.sethook() -- Disable.
		-- error("Script timeout", 0)
		-- error("Run timeout", 0)
		error("Run timeout{E6A0C4BD-75DC-4313-A1AF-7666ED28545B}", 0)
	end
end

function _sandboxMaxHook(maxInstructions)
	local max = maxInstructions
	local hook = _sandboxhook
	if type(maxInstructions) == "table" then
		max = maxInstructions.max
		if maxInstructions.hook then
			hook = function(a, b)
				return _sandboxhook(a, b, maxInstructions.hook)
			end
		end
	end
	assert(not max or max > 0)
	return max or 10000, hook
end


-- Returns (true, env) on success, or (false, message) on failure.
function runSandboxHook(code, maxInstructions, env, beforeRunFunc)
	env = env or {}
	local coro, y = loadSandboxHook(code, maxInstructions, env, beforeRunFunc)
	if coro then
		local a, b = coroutine.resume(coro)
		if not a then
			return false, b
		end
		return true, env
	end
	return false, y
end


-- Returns (thread, env) on success, or (nil, message) on failure.
function loadSandboxHook(code, maxInstructions, env, beforeRunFunc)
	local max, hook = _sandboxMaxHook(maxInstructions)
	-- Shouldn't need to pcall runSandboxFull.
	local coro, y = loadSandboxFull(code, env, beforeRunFunc)
	if coro then
		debug.sethook(coro, hook, "", 10 + max)
		-- Note: now with coroutines, don't need to clear the debug hook.
		return coro, y
	end
	return nil, y
end


function threadHook(thread, maxInstructions)
	local max, hook = _sandboxMaxHook(maxInstructions)
	debug.sethook(thread, hook, "", 10 + max)
end


-- Environment not sandboxed! Just limited instructions!
-- Note: if code is a function already with a sandboxed env, then the env is still sandboxed.
-- Note: a sandbox thread yield/resume do not need this function.
-- Returns (true, result) on success, or (false, message) on failure.
function runHook(code, maxInstructions, ...)
	local max, hook = _sandboxMaxHook(maxInstructions)
	local oldenv = false
	if type(code) == "string" then
		local x, y = loadstring(" \t " .. code .. " \t ", "user")
		if not x then
			return false, y
		end
		code = x
	end
	debug.sethook(hook, "", 10 + max)
	local x, y
	if coroutine.running() then
		x, y = true, code(...)
	else
		x, y = pcall(code, ...)
	end
	debug.sethook() -- Disable.
	return x, y
end


function checkfatal(...)
	-- Note: depends on lua throwing 'not enough memory'
	local a, b = ...
	if not a then
		if type(b) == "string" then
			if b:find("{E6A0C4BD-75DC-4313-A1AF-7666ED28545B}", 1, true)
					or b == "not enough memory" then
				error(b, 0)
			end
		end
	end
	return ...
end


-- env is optional, can be used to pre-set values in the environment.
-- Note: env should not be or contain any tables used elsewhere, as they can be modified.
-- Returns (true, env) on success, or (false, message) on failure.
-- On success, the return value from the code is stored in env["return"]
function runSandboxFull(code, env, beforeRunFunc)
	--[[local oldenv = false
	if type(code) == "function" then
		oldenv = getfenv(code)
	end--]]
	local coro, env = loadSandboxFull(code, env, beforeRunFunc)
	if not sandboxed then
		return false, env -- env=err
	end
	local x, y = coroutine.resume(coro)
	--[[if oldenv ~= false then
		setfenv(sandboxed, oldenv)
	end--]]
	if not x then
		return nil, y
	end
	env["return"] = y
	return true, env
end

function createSandboxEnv(env)
	env = env or {}
	env._G = env
	env._VERSION = env._VERSION or _VERSION or "Lua 5.1"
	-- env.print = env.print or print
	env.print = env.print or _null_print
	env.assert = env.assert or assert
	env.error = env.error or error
	-- env.error = env.error or (function(msg) error(msg, 0) end)
	env.ipairs = env.ipairs or ipairs
	env.pairs = env.pairs or pairs
	env.next = env.next or next
	env.select = env.select or select
	env.tonumber = env.tonumber or tonumber
	env.tostring = env.tostring or tostring
	env.type = env.type or type
	env.string = env.string or {} -- table
	env.string.byte = env.string.byte or string.byte
	env.string.char = env.string.char or string.char
	env.string.find = env.string.find or string.find
	env.string.format = env.string.format or string.format
	env.string.gmatch = env.string.gmatch or string.gmatch
	env.string.gsub = env.string.gsub or string.gsub
	env.string.len = env.string.len or string.len
	env.string.lower = env.string.lower or string.lower
	env.string.match = env.string.match or string.match
	env.string.rep = env.string.rep or string.rep
	env.string.reverse = env.string.reverse or string.reverse
	env.string.sub = env.string.sub or string.sub
	env.string.upper = env.string.upper or string.upper
	env.table = env.table or {} -- table
	env.table.insert = env.table.insert or table.insert
	env.table.remove = env.table.remove or table.remove
	env.table.concat = env.table.concat or table.concat
	-- env.table.maxn = env.table.maxn or table.maxn
	env.table.sort = env.table.sort or table.sort
	env.table.unpack = env.table.unpack or table.unpack or env.unpack or unpack
	env.unpack = env.table.unpack
	env.math = env.math or {} -- table
	env.math.abs = env.math.abs or math.abs
	env.math.acos = env.math.acos or math.acos
	env.math.asin = env.math.asin or math.asin
	env.math.atan = env.math.atan or math.atan
	env.math.atan2 = env.math.atan2 or math.atan2
	env.math.ceil = env.math.ceil or math.ceil
	env.math.cos = env.math.cos or math.cos
	env.math.cosh = env.math.cosh or math.cosh
	env.math.deg = env.math.deg or math.deg
	env.math.exp = env.math.exp or math.exp
	env.math.floor = env.math.floor or math.floor
	env.math.fmod = env.math.fmod or math.fmod
	env.math.frexp = env.math.frexp or math.frexp
	env.math.huge = env.math.huge or math.huge
	env.math.ldexp = env.math.ldexp or math.ldexp
	env.math.log = env.math.log or math.log
	env.math.log10 = env.math.log10 or math.log10
	env.math.max = env.math.max or math.max
	env.math.min = env.math.min or math.min
	env.math.modf = env.math.modf or math.modf
	env.math.pi = env.math.pi or math.pi
	env.math.pow = env.math.pow or math.pow
	env.math.rad = env.math.rad or math.rad
	env.math.random = env.math.random or math.random
	env.math.sin = env.math.sin or math.sin
	env.math.sinh = env.math.sinh or math.sinh
	env.math.sqrt = env.math.sqrt or math.sqrt
	env.math.tan = env.math.tan or math.tan
	env.math.tanh = env.math.tanh or math.tanh
	env.os = env.os or {} -- table
	env.os.clock = env.os.clock or os.clock
	env.os.date = env.os.date or function(fmt, t)
		if type(fmt) == "string" then
			-- Need to avoid crashing bug.
			-- Widely supported flags based on common OSes:
			local allow = { ['a']=1,['A']=1,['b']=1,['B']=1,['c']=1,['d']=1,['F']=1,
				['H']=1,['I']=1,['j']=1,['m']=1,['M']=1
				,['p']=1,['S']=1,['U']=1,['w']=1,['W']=1,['x']=1,['X']=1,
				['y']=1,['Y']=1,['z']=1,['Z']=1,['%']=1,}
			fmt = fmt:gsub("%%([#]?)(.)", function(pnd, ch)
				if not allow[ch] then
					error("Unsupported format: %" .. pnd .. ch)
				end
				return '%' .. pnd .. ch
			end)
		end
		return os.date(fmt, t)
	end
	env.os.difftime = env.os.difftime or os.difftime
	env.os.time = env.os.time or os.time
	env.os.exit = env.os.exit or function(code)
		code = math.floor(code or 0)
		error("exit " .. code .. "{E6A0C4BD-75DC-4313-A1AF-7666ED28545B}", 0)
	end
	env.loadstring = env.loadstring or function(src, name)
		local f, err = checkfatal(loadstring(" \t " .. src .. " \t ", name or "user.loadstring"))
		if not f then
			return nil, err
		end
		setfenv(f, env)
		return f
	end
	env.package = env.package or {}
	env.package.config = "/\n;\n?\n!\n-"
	env.package.cpath = ""
	env.package.loaded = {} -- loaded modules
	env.package.loadlib = function(libname, funcname)
		return nil, "loadlib is disabled", "disabled"
	end
	env.package.searchpath = function(name, path, sep, rep)
		return nil, "package.searchpath is disabled"
	end
	env.package.path = ""
	env.package.preload = {} -- functions
	env.package.seeall = function(m)
		-- Try to emulate package.seeall depending on the metatable support in env:
		if not env.getmetatable or not env.setmetatable then
			error('package.seeall error: depends on getmetatable and setmetatable', 2)
		end
		if type(m) ~= "table" then
			error("table expected, got " .. type(m))
		end
		local mt = env.getmetatable(m) or {}
		mt.__index = env
		env.setmetatable(m, mt)
	end
	env.package.searchers = {} -- (Lua 5.2 renamed from package.loaders)
	env.package.loaders = env.package.searchers -- functions (Lua 5.1)
	-- Adding the default searchers as documented. Some do nothing for sandbox security.
	table.insert(env.package.searchers, function(modname)
		-- The first searcher simply looks for a loader in the package.preload table.
		return env.package.preload[modname]
	end)
	table.insert(env.package.searchers, function(modname)
		-- The second searcher looks for a loader as a Lua library, using the path stored
		-- at package.path. The search is done as described in function package.searchpath.
		--[[
		-- Don't bother if env.package.path is nil or empty...
		-- Note: if not env.dofile then return nil...
		local sp = env.package.searchpath(modname, env.package.path)
		if sp then
			return function(modname, path)
				-- env.dofile...
			end
		end
		--]]
    return function(modname, ...)
      -- Trick to require the standard modules,
      if modname == "math" then
        return env.math
      elseif modname == "string" then
        return env.string
      elseif modname == "package" then
        return env.package
      elseif modname == "os" then
        return env.os
      elseif modname == "io" then
        return env.io
      elseif modname == "debug" then
        return env.debug
      elseif modname == "table" then
        return env.table
      elseif modname == "coroutine" then
        return env.coroutine
      end
    end
	end)
	table.insert(env.package.searchers, function(modname)
		-- The third searcher looks for a loader as a C library, using the path given by
		-- the variable package.cpath. ... described in function package.searchpath.
		--[[
		-- Don't bother if env.package.cpath is nil or empty...
		local sp = env.package.searchpath(modname, env.package.cpath)
		if sp then
			return function(modname, path)
				return env.package.loadlib(path, "luaopen_" .. modname:gsub('%.', '_'))
			end
		end
		--]]
	end)
	table.insert(env.package.searchers, function(modname)
		-- The fourth searcher tries an all-in-one loader.
		-- Not implemented.
	end)
	env.require = function(modname)
		local reasons = nil
		if env.package then
			if type(env.package.loaded) == "table" then
				local m = env.package.loaded[modname]
				if m then
					return m
				end
			end
			if type(env.package.searchers) == "table" then
				for i, v in ipairs(env.package.searchers) do
					local f, farg = v(modname)
					if type(f) == "string" then
						-- Can explain why it didn't find it, to add to the error message...
						if reasons then
							table.insert(reasons, f)
						else
							reasons = { "", f }
						end
					elseif type(f) == "function" then
						local m = f(modname, farg)
						if m then
							env.package.loaded[modname] = m
							return m
						end
					end
				end
			end
		end
		local err = 'require cannot find module ' .. modname
		if reasons then
			reasons[1] = err
			error(table.concat(reasons, "\n\t"), 2)
		end
		error(err, 2)
	end
	--[[
	-- WARNING: can be used to break out of CPU and MEMORY limits!
	local prevpcall = env.pcall or pcall
	env.pcall = function(f, ...)
		-- important, force environment of f:
		-- don't think this is compatible with newest lua!
		local f2 = function(...)
			return f(...)
		end
		setfenv(f2, env)
		return prevpcall(f2, ...)
	end
	--]]
	--[[
	env.pcall = env.pcall or function(f, ...)
		-- FAKE PCALL FOR COMPAT:
		return true, f(...)
	end
	--]]
  -- [[ -- This pcall is STILL DANGEROUS due to infinite recursion calling pcall!
  -- Example:    local x; x = function() return pcall(x) end; x() -- CORE DUMP!
  -- Might be only a core dump with coco lua coroutines.
  -- For now let's just have a max number of allowed pcalls per sandbox.
  local npcalls = 0
	local prevpcall = env.pcall or pcall
	env.pcall = function(f, ...)
    if npcalls >= 20 then
      -- return nil, "Security error with pcall"
      -- Instead of crapping out, call directly:
      return true, f(...)
    end
    npcalls = npcalls + 1
		-- important, force environment of f:
		-- don't think this is compatible with newest lua!
		-- Don't remove this env wrapper function. See SandBoxes wiki: pcall, loadstring.
		-- loadstring will use the caller's env, which can be global in the case of loadstring.
		local f2 = function(...)
			return f(...)
		end
		setfenv(f2, env)
		return checkfatal(prevpcall(f2, ...))
	end--]]
	return env
end

-- Returns (thread, env) on success, or (nil, message) on failure.
function loadSandboxFull(code, env, beforeRunFunc)
	local sandboxed
	if type(code) == "string" then
		local x, y = loadstring(" \t " .. code .. " \t ", "user")
		if not x then
			return nil, y
		end
		sandboxed = x
	else
		-- -- Note: if code is a function, its environment is changed and not reverted.
		error("Sandbox code must be a string", 2) -- Avoid that problem.
		sandboxed = code
		oldenv = getfenv(code)
	end

	env = env or {}
	createSandboxEnv(env)
	setfenv(sandboxed, env)

	if beforeRunFunc then
		beforeRunFunc(env, sandboxed)
	end
	return coroutine.create(sandboxed), env
end

