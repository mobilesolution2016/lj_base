local ffi = require('ffi')
local _isFile = 0
local _isDir = 0
local _isExists = 0
local _openDir = 0
local _createDir = 0
local _getAttr = 0
local _checkAttr = 0

local dirt = {}
local mode2n = function(mode)
	if type(mode) == 'string' and (#mode == 3 or #mode == 4) then
		local s, g, o = string.byte(string.byte(mode) == 48 and string.sub(mode , 2) or mode, 1, 3)
		s = bit.lshift(s - 48, 6)
		g = bit.lshift(g - 48, 3)
		return bit.bor(bit.bor(s, g), o - 48)
	end
end

local pathsegs = table.new(6, 0)
local mergePath = function(self)
	if self.driver then pathsegs[1] = self.driver .. ':' end
	if self.path then pathsegs[#pathsegs + 1] = self.path end
	if self.name then pathsegs[#pathsegs + 1] = self.name end
	if self.ext then pathsegs[#pathsegs + 1] = self.ext end
	pathsegs[#pathsegs + 1] = nil
	return table.concat(pathsegs, '/')
end

ffi.cdef[[
	typedef struct _LocalDateTime
	{
		uint16_t	year, month, day, dayofweek;
		uint16_t	hour, minute, second, millisecond;
	} LocalDateTime;
	
	int deleteDirectory(const char* path);
	int deleteFile(const char* fname);
	
	bool getFileTime(const char* fname, LocalDateTime* create, LocalDateTime* update);
	double getFileSize(const char* fname);
]]

if ffi.os == 'Windows' then
	ffi.cdef[[
		int PathFileExistsA(const char*);
		int PathIsDirectoryA(const char*);

		typedef struct _FILETIME {
			unsigned long dwLowDateTime;
			unsigned long dwHighDateTime;
		} FILETIME;

		typedef struct _WIN32_FIND_DATA {
			unsigned	dwFileAttributes;
			FILETIME	ftCreationTime;
			FILETIME	ftLastAccessTime;
			FILETIME	ftLastWriteTime;
			unsigned	nFileSizeHigh;
			unsigned	nFileSizeLow;
			unsigned	dwReserved0;
			unsigned	dwReserved1;
			char		cFileName[260];
			char		cAlternateFileName[14];
		} WIN32_FIND_DATAA;

		void* __stdcall FindFirstFileA(const char*, WIN32_FIND_DATAA*);
		int __stdcall FindNextFileA(void*, WIN32_FIND_DATAA*);
		int __stdcall FindClose(void* hFindFile);
		int __stdcall CreateDirectoryA(const char*, void*);
		unsigned __stdcall GetFileAttributesA(const char*);
	]]

	local kernel32 = ffi.load('kernel32.dll')
	local shlwapi = ffi.load('shlwapi.dll')
	
	local FILE_ATTRIBUTE_TEMPORARY = 0x100
	local FILE_ATTRIBUTE_ARCHIVE = 0x20
	local FILE_ATTRIBUTE_DIRECTORY = 0x10
	local FILE_ATTRIBUTE_HIDDEN = 0x2
	local FILE_ATTRIBUTE_READONLY = 0x1
	local FILE_ATTRIBUTE_EXECUTE = 0x80000000
	
	local executes = { exe = 1, com = 1, bat = 1 }
	
	local attrChecks = {
		file = function(val)
			return bit.band(val, FILE_ATTRIBUTE_ARCHIVE) ~= 0 and true or false
		end,
		dir = function(val)
			return bit.band(val, FILE_ATTRIBUTE_DIRECTORY) ~= 0 and true or false
		end,
		hidden = function(val)
			return bit.band(val, FILE_ATTRIBUTE_HIDDEN) ~= 0 and true or false
		end,
		link = function(val)
			return false
		end,
		temporary = function(val)
			return bit.band(val, FILE_ATTRIBUTE_TEMPORARY) ~= 0 and true or false
		end,
		socket = function(val)
			return false
		end,
	}

	_isFile = function(path)
		return shlwapi.PathFileExistsA(path) ~= 0
	end
	_isDir = function(path)
		return shlwapi.PathIsDirectoryA(path) ~= 0
	end
	_isExists = function(path)
		if shlwapi.PathFileExistsA(path) ~= 0 then
			return 1
		end
		if shlwapi.PathIsDirectoryA(path) ~= 0 then
			return 2
		end
	end
	
	_getAttr = function(path)
		local r = kernel32.GetFileAttributesA(path)
		if r ~= 0xFFFFFFFF then
			local fext = string.rfindchar(path, '.')
			if fext then
				fext = string.sub(path, fext + 1)
				if fext and executes[string.lower(fext)] then					
					r = bit.bor(r, FILE_ATTRIBUTE_EXECUTE)
				end
			end
			
			return r
		end
	end
	_checkAttr = function(path, what)
		if type(what) == 'string' then
			local val = type(path) == 'string' and kernel32.GetFileAttributesA(path) or path
			if val ~= 0xFFFFFFFF then
				local f = attrChecks[what]
				if f then return f(val) end
				
				if #what == 3 or #what == 4 then
					local r = true
					local s, g, o = string.byte(string.byte(what) == 48 and string.sub(what , 2) or what, 1, 3)

					if (bit.band(s, 2) ~= 0 or bit.band(g, 2) ~= 0 or bit.band(o, 2) ~= 0) and bit.band(val, FILE_ATTRIBUTE_READONLY) ~= 0 then
						r = false
					end
					if (bit.band(s, 1) ~= 0 or bit.band(g, 1) ~= 0 or bit.band(o, 1) ~= 0) and bit.band(val, FILE_ATTRIBUTE_EXECUTE) == 0 then
						r = false
					end
					
					return r
				end
			end
		end
	end

	_openDir = function(path, filter)
		local last = path:sub(#path)
		local r = { (last == '\\' or last == '/') and path or (path .. '/'), ffi.new('WIN32_FIND_DATAA') }

		r[3] = kernel32.FindFirstFileA(r[1] .. (filter == nil and '*.*' or filter), r[2])
		if r[3] == nil then
			r[2] = nil
			return nil
		end

		return setmetatable(r, dirt)
	end

	dirt.__index = {
		pick = function(self)
			if not self[4] or kernel32.FindNextFileA(self[3], self[2]) ~= 0 then
				self[4] = ffi.string(self[2].cFileName)
				self[5] = self[2].dwFileAttributes
				return true
			end

			return false
		end,
		close = function(self)
			if self[3] then
				kernel32.FindClose(self[3])
				self[3], self[2] = nil, nil
			end
		end,
		name = function(self)
			return self[4]
		end,
		fullname = function(self)
			return self[1] .. self[4]
		end,
		
		getAttrs = function(self)
			return self[5]
		end,
	}

	_createDir = function(path, mode)
		return kernel32.CreateDirectoryA(path, nil) ~= 0
	end

else
	ffi.cdef[[
		void* opendir(const char* path);
		int closedir(void* handle);

		const char* readdirinfo(void* handle, const char* filter);
		bool pathisfile(const char* path);
		bool pathisdir(const char* path);
		unsigned pathisexists(const char* path);
		bool createdir(const char* path, int mode);
		unsigned getpathattrs(const char* path);
	]]

	local readdirinfo = _G.libreemext.readdirinfo
	local getpathattrs = _G.libreemext.getpathattrs
	local pathisexists = _G.libreemext.pathisexists
	local createdir = _G.libreemext.createdir
	
	local FILE = 1
	local DIR = 2
	local LINK = 4
	local SOCKET = 8
	local HIDDEN = 16
	local O_READ, O_WRITE, O_EXEC = 256, 128, 64
	local G_READ, G_WRITE, G_EXEC = 32, 16, 8
	local R_READ, R_WRITE, R_EXEC = 4, 2, 1

	local attrChecks = {
		file = function(val)
			return bit.band(val, FILE) ~= 0 and true or false
		end,
		dir = function(val)
			return bit.band(val, DIR) ~= 0 and true or false
		end,
		hidden = function(val)
			return bit.band(val, HIDDEN) ~= 0 and true or false
		end,
		link = function(val)
			return bit.band(val, LINK) ~= 0 and true or false
		end,
		temporary = function(val)
			return false
		end,
		socket = function(val)
			return bit.band(val, SOCKET) ~= 0 and true or false
		end,
	}
	
	_isFile = _G.libreemext.pathisfile
	_isDir = _G.libreemext.pathisdir
	_isExists = function(path)
		local r = pathisexists(path)
		if r ~= 0 then
			return r
		end
	end

	_getAttr = function(path)
		return getpathattrs(path)
	end
	_checkAttr = function(path, what)
		if type(what) == 'string' then
			local val = type(path) == 'string' and getpathattrs(path) or path
			if val ~= 0 then
				local f = attrChecks[what]
				if f then  return f(val) end
				
				f = mode2n(what)
				return (f and bit.band(val, f) == f) and true or false
			end
		end
	end

	_openDir = function(path, filter)
		local handle = ffi.C.opendir(path)
		if handle == nil then
			return nil
		end

		local r = table.new(4, 0)
		local last = path:sub(#path)

		if last == '\\' then
			r[2] = string.sub(path, 1, -2) .. '/'
		elseif last ~= '/' then
			r[2] = path .. '/'
		else
			r[2] = path
		end
		r[1] = handle
		r[3] = (type(filter) == 'string' and #filter > 0) and filter or '*'

		return setmetatable(r, dirt)
	end

	dirt.__index = {
		pick = function(self)
			if self[1] ~= nil then
				local name = readdirinfo(self[1], self[3])
				if name ~= nil then
					self[4] = ffi.string(name)
					self[5] = nil
					return true
				end
			end
			return false
		end,
		close = function(self)
			if self[1] ~= nil then
				ffi.C.closedir(self[1])
				self[1], self[4] = nil, nil
			end
		end,
		name = function(self)
			return self[4] or ''
		end,
		fullname = function(self)
			return self[2] .. (self[4] or '')
		end,
		
		getAttrs = function(self)
			if not self[5] then
				self[5] = getpathattrs(self[2] .. (self[4] or ''))
			end
			return self[5]
		end,
	}

	_createDir = function(path, mode)
		return createdir(path, mode2n(mode) or 0)
	end
end

local _getFileSize = _G.libreemext.getFileSize
local _getFileTime = _G.libreemext.getFileTime
local createTime, updateTime = ffi.new('LocalDateTime'), ffi.new('LocalDateTime')

return {
	--���·���Ƿ��Ǹ��ļ�
	pathIsFile = _isFile,
	--���·���Ƿ��Ǹ�Ŀ¼
	pathIsDir = _isDir,
	--���·���Ƿ���ڣ��ļ�=1����Ŀ¼=2��������nil˵����·�������ڣ��ļ�Ҳ����Ŀ¼Ҳ���ǣ�
	pathIsExists = _isExists,

	--��ȡ����ֵ��Ȼ���������checkAttr�����Ĳ���1���������Լ���ͬһ��path���checkAttrʱ��IO��������
	getAttr = _getAttr,

	--checkAttr(path, checkWhat)
	--path������һ��·����Ҳ������getAttr�����ķ���ֵ
	--what�����ǣ�file|dir|hidden|temporary|link|socket������777��755��422��511�ȵȣ�����8���Ʊ�ʾ��Ȩ�ޣ�����ע��Ҫ�����ַ��������������֣���ΪLuaԭ����֧��0777����8��������
	checkAttr = _checkAttr,

	--openDir(path[, filter])
	--filterΪ�ļ�����������ͨ����������ã���a.*����xxx*��Ҳ����û�У���ָ��Ϊ*|.|*.*�Ͳ�ָ����û������ģ������粻ָ��
	openDir = _openDir,

	--֧�ֶ༶����
	createDir = function(path, mode, recur)
		if recur then
			local s, drvLet = 0, false
			local first = path:sub(1, 1)
			local segs = string.split(path, '/\\')
			local newpath = (first == '\\' or first == '/') and first or ''

			if ffi.os == 'Windows' and #segs[1] == 2 and string.byte(segs[1], 2) == 58 then
				drvLet = true
			end

			for i = 1, #segs do
				local chk = newpath .. segs[i]
				if (drvLet and i == 1) or _isDir(chk) then
					newpath = chk .. '/'
				else
					s = i
					break
				end
			end

			if s ~= 0 then
				for i = s, #segs do
					newpath = newpath .. segs[i]
					if not _createDir(newpath, mode) then
						return false
					end

					newpath = newpath .. '/'
				end
			end

			return true
		end

		return _createDir(path, mode)
	end,
	--֧�ֶ༶ɾ��
	deleteDir = function(dir)
		return libreemext.deleteDirectory(dir) ~= 0
	end,

	deleteFile = function(dir)
		return libreemext.deleteFile(dir) ~= 0
	end,

	--�����ļ�·��ȡ�ļ��ߴ磬ʧ�ܷ���nil
	filesize = function(path)
		local s = _getFileSize(path)
		if s ~= -1 then return s end
	end,
	
	--����·��ȡ�ļ�����������޸�ʱ�䣬���ش����͸���ʱ�䣬ʧ�ܷ�������nil
	filetime = function(path)
		local c, u = ffi.new('LocalDateTime'), ffi.new('LocalDateTime')
		if _getFileTime(path, c, u) then
			return c, u
		end
	end,
	--����ȡ�ļ��ĸ���ʱ��
	fileutime = function(path)
		local u = ffi.new('LocalDateTime')
		if _getFileTime(path, nil, u) then
			return u
		end
	end,
	
	--����·��ȡ�ļ�����������޸�ʱ�䣬���ش����͸���ʱ�����ʧ�ܷ�������nil
	filets = function(path)
		if _getFileTime(path, createTime, updateTime) then
			return os.time({year = createTime.year, month = createTime.month, day = createTime.day, hour = createTime.hour, min = createTime.minute, sec = createTime.second}),
				   os.time({year = updateTime.year, month = updateTime.month, day = updateTime.day, hour = updateTime.hour, min = updateTime.minute, sec = updateTime.second})
		end
	end,
	--����ȡ�ļ��ĸ���ʱ���ʱ���
	fileuts = function(path)
		if _getFileTime(path, nil, updateTime) then
			return os.time({year = updateTime.year, month = updateTime.month, day = updateTime.day, hour = updateTime.hour, min = updateTime.minute, sec = updateTime.second})
		end
	end,
	
	--ȡ�ļ���չ����δ�ɹ�����nil
	fileext = function(path)
		local ext = string.rfindchar(path, '.')
		if ext then
			return string.lower(string.sub(path, ext + 1))
		end
	end,
	
	--�ֽ�·��
	pathinfo = function(path)
		local r = table.new(0, 6)
		local namepos = string.rfindchar(path, '/', 1, true)
		if not namepos then
			namepos = string.rfindchar(path, '\\', 1, true)
		end
		
		if namepos then
			r.path = string.sub(path, 1, namepos)
			r.name = string.sub(path, namepos + 1)
			if #r.name > 1 then
				local ext = string.rfindchar(r.name, '.')
				if ext then
					r.ext = string.lower(string.sub(r.name, ext + 1))
					r.name = string.sub(r.name, 1, ext)
				end
			end
		else
			r.path = path
		end
		
		if ffi.os == 'Windows' then
			local second, third = string.byte(r.path, 2, 3)
			if second == 58 and (third == 47 or third == 92) then
				r.driver = string.sub(r.path, 1, 1)
				r.path = string.sub(r.path, 4)
			end
		end
		
		r.merge = mergePath

		return r
	end,
}