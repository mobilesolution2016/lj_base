local ffi = require('ffi')

--some CRT/Win32API functions
ffi.cdef[[
	int strcspn(const char*, const char*);
	int strspn(const char*, const char*);
]]

if ffi.os == 'Windows' then
	ffi.cdef [[
		int strcmp(const char*, const char*);
		int strcasecmp(const char*, const char*) __asm__("_stricmp");
		int strncmp(const char*, const char*, size_t);
		int strncasecmp(const char*, const char*, size_t) __asm__("_strnicmp");

		int access(const char *, int) __asm__("_access");
		
		unsigned __stdcall GetCurrentThreadId();
		void __stdcall Sleep(unsigned dwMilliseconds);
	]]
	
	local kernel32 = ffi.load('kernel32.dll')
	_G.os.threadId = function()
		return kernel32.GetCurrentThreadId()
	end
	_G.os.sleep = function(millisecs)
		return kernel32.Sleep(millisecs)
	end
else
	ffi.cdef [[
		int strcmp(const char*, const char*);
		int strcasecmp(const char*, const char*);
		int strncmp(const char*, const char*, size_t);
		int strncasecmp(const char*, const char*, size_t);

		int access(const char *, int);
		
		unsigned long pthread_self();
		void usleep(size_t usecs);
	]]	

	_G.os.threadId = function()
		return ffi.C.pthread_self()
	end
	_G.os.sleep = function(millisecs)
		return kernel32.usleep(millisecs * 1000)
	end
end


--lua standard library extends
local tablelib = _G.table
--将tbl中的元素构造成一个全新的唯一值的数组返回
tablelib.unique = function(tbl)
	local cc = #tbl
	local s, r = table.new(0, cc), table.new(cc, 0)
	for i = 1, cc do
		s[tbl[i]] = true
	end
	local i = 1
	for k,_ in pairs(s) do
		r[i] = k
		i = i + 1
	end
	return r
end
--将A数组中的元素如果在B数组存在有的就移除，剩下的不存在于B数组中的构建出一个新的数组返回
--不支持key=>value型的Table
tablelib.exclude = function(A, B)
	if type(A) == 'table' and type(B) == 'table' then
		local r, uniques = table.new(#A / 2, 0), B
		
		if #B > 0 then
			uniques = table.new(0, #B)
			for i = 1, #B do
				uniques[B[i]] = true
			end
		end
		
		local n = 1
		for i = 1, #A do
			if not B[A[i]] then
				r[n] = A[i]
				n = n + 1
			end
		end
		
		return r
	end
end
--将数组A和B中共有的元素拿出来组成一个新的数组返回
--不支持key=>value型的Table
tablelib.union = function(A, B)
	if type(A) == 'table' and type(B) == 'table' then
		local r, uniques = table.new(8, 0), table.new(0, 8)
		
		for i = 1, #A do
			uniques[A[i]] = 1
		end
		for i = 1, #B do
			uniques[B[i]] = 1
		end
		
		local n = 1
		for k,v in pairs(uniques) do
			r[n] = k
			n = n + 1
		end
		
		return r
	end
end
--在数组A后面追加任意多个数组，支持key=>value和数组型Table
tablelib.append = function(A, ...)
	if type(A) == 'table' then
		for i = 1, select('#', ...) do
			local B = select(i, ...)
			if type(B) == 'table' then
				if #B > 0 then
					local s = #A
					for k = 1, #B do
						A[s + k] = B[k]
					end
				else
					for k,v in pairs(B) do
						A[k] = v
					end
				end
			end
		end
		
		return A
	end
end
--查找在self中v是否存在，存在的话返回k，否则返回nil。支持数组和key=>value型Table
tablelib.exists = function(self, v)
	if type(self) == 'table' then
		local num = #self
		if num > 0 then
			for i = 1, num do
				if self[i] == v then
					return i
				end
			end
		else
			for k, cmp in pairs(self) do
				if cmp == v then
					return k
				end
			end
		end	
	end
end

--IO库扩展
local iolib = _G.io
iolib.exists = function(name)
	return ffi.C.access(name, 0) == 0
end
iolib.dumpto = function(fname, data)
	local fp = io.open(fname, 'wb')
	if fp then
		fp:write(data)
		fp:close()
		return true
	end
	return false
end
iolib.fullfrom = function(fname)
	local fp = io.open(fname, 'rb')
	if fp then
		local data = fp:read('*a')
		fp:close()
		return data
	end
end


--字符串库的其它扩展函数
local strlib = _G.string
strlib.cut = function(str, p)
	if str then
		local pos = string.find(str, p, 1, true)
		if pos then
			return string.sub(str, 1, pos - 1), string.sub(str, pos + 1)
		end
	end
	return str
end
strlib.casecmp = function(a, b)
	return ffi.C.strcasecmp(a, b) == 0
end
strlib.ncasecmp = function(a, b, n)
	return ffi.C.strncasecmp(a, b, n) == 0
end
strlib.cmp = function(a, b, c, d)
	local func
	if type(c) == 'number' then
		func = d == true and ffi.C.strncasecmp or ffi.C.strncmp
		return func(a, b, c) == 0
	else
		func = c == true and ffi.C.strcasecmp or ffi.C.strcmp
		return func(a, b) == 0
	end

	return false
end
strlib.spn = ffi.C.strspn
strlib.cspn = ffi.C.strcspn